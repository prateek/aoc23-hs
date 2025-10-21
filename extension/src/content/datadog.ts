import type { Config, Route, Asset } from '../core/schema';
import { maybeHashAsset } from '../core/utils';
import { redactAndTruncateQuery } from '../core/redactor';
import {
  PerformanceTracker,
  DatadogWidgetDetector,
  type PanelDetectionResult,
} from '../core/performance';

export interface DatadogPageContext {
  pageType: string;
  asset?: Partial<Asset>;
}

export class DatadogTracker {
  constructor(private config: Config) {}

  // Detect page type and context from URL and DOM
  async detectPageContext(route: Route): Promise<DatadogPageContext> {
    const path = route.path;

    // Dashboards: /dashboard/:id or /dashboard/lists
    if (path.includes('/dashboard/')) {
      const dashboardMatch = path.match(/\/dashboard\/([^/]+)/);
      if (dashboardMatch && dashboardMatch[1] !== 'lists') {
        const dashboardId = dashboardMatch[1];
        const asset: Partial<Asset> = {
          type: 'dashboard',
          uid: await maybeHashAsset(dashboardId, this.config, true),
        };

        // Try to get dashboard name from DOM
        const titleEl = document.querySelector('[data-testid="dashboard-title"], .dashboard-title');
        if (titleEl?.textContent) {
          asset.name = await maybeHashAsset(titleEl.textContent.trim(), this.config, true);
        }

        return {
          pageType: 'dashboard',
          asset,
        };
      }
    }

    // Logs: /logs
    if (path.includes('/logs')) {
      return {
        pageType: 'logs',
        asset: { type: 'logs' },
      };
    }

    // APM/Traces: /apm/* or /traces/*
    if (path.includes('/apm') || path.includes('/traces')) {
      return {
        pageType: 'apm',
      };
    }

    // Monitors: /monitors/:id
    if (path.includes('/monitors')) {
      const monitorMatch = path.match(/\/monitors\/(\d+)/);
      if (monitorMatch) {
        const monitorId = monitorMatch[1];
        const asset: Partial<Asset> = {
          type: 'monitor',
          uid: await maybeHashAsset(monitorId, this.config, true),
        };

        return {
          pageType: 'monitors',
          asset,
        };
      }

      return {
        pageType: 'monitors',
        asset: { type: 'monitor' },
      };
    }

    // Synthetics: /synthetics/*
    if (path.includes('/synthetics')) {
      return {
        pageType: 'synthetics',
      };
    }

    // Default
    return {
      pageType: 'unknown',
    };
  }

  // Setup query detection for Datadog
  setupQueryDetection(onQuery: (query: string) => void): void {
    let lastQueryTime = 0;

    // Listen for clicks on search/query buttons
    document.addEventListener('click', (e) => {
      const target = e.target as HTMLElement;

      // Look for search or query buttons
      if (
        target.textContent?.includes('Search') ||
        target.closest('[data-testid*="search"]') ||
        target.closest('[aria-label*="Search"]') ||
        target.closest('.search-button')
      ) {
        const now = Date.now();
        // Throttle to max 1 query event per 5s
        if (now - lastQueryTime < 5000) return;
        lastQueryTime = now;

        // Try to extract query from search input
        const searchInput = document.querySelector('input[type="search"], [data-testid="search-input"]') as HTMLInputElement;
        const query = searchInput?.value || '';
        onQuery(query);
      }
    }, true);

    // Also listen for Enter key in search inputs
    document.addEventListener('keydown', (e) => {
      if (e.key !== 'Enter') return;

      const target = e.target as HTMLElement;
      if (target.tagName === 'INPUT' && (target.getAttribute('type') === 'search' || target.closest('[data-testid*="search"]'))) {
        const now = Date.now();
        if (now - lastQueryTime < 5000) return;
        lastQueryTime = now;

        const query = (target as HTMLInputElement).value || '';
        onQuery(query);
      }
    }, true);
  }

  // Build query metadata for event with redaction
  async buildQueryMetaForEvent(query: string): Promise<{ text?: string; tokens?: string[]; length: number; redaction_applied: boolean } | undefined> {
    if (!query) return undefined;

    const meta: { text?: string; tokens?: string[]; length: number; redaction_applied: boolean } = {
      length: query.length,
      redaction_applied: false,
    };

    if (this.config.privacy_toggles.collect_raw_queries) {
      const redacted = redactAndTruncateQuery(query, this.config.redaction_policy, 500);
      meta.text = redacted.text;
      meta.redaction_applied = redacted.applied;

      // Tokenize
      meta.tokens = redacted.text
        .split(/\s+/)
        .filter((t) => t.length > 0 && t.length <= 64);
    }

    return meta;
  }

  // Try to detect user identity from DOM (opt-in)
  detectIdentityFromDOM(): { name?: string; email?: string } | null {
    if (!this.config.privacy_toggles.dom_identity_scrape) {
      return null;
    }

    // Try to find user menu or profile elements
    // This is highly dependent on Datadog's DOM structure and may be fragile
    const userMenu = document.querySelector('[data-testid="user-menu"], .user-menu');
    const nameEl = userMenu?.querySelector('[data-testid="user-name"], .user-name');
    const emailEl = userMenu?.querySelector('[data-testid="user-email"], .user-email');

    const name = nameEl?.textContent?.trim();
    const email = emailEl?.textContent?.trim();

    if (name || email) {
      return { name, email };
    }

    return null;
  }

  // Setup performance tracking for dashboard/widget loads
  setupPerformanceTracking(
    performanceTracker: PerformanceTracker,
    onWaterfall: (waterfall: any) => void
  ): () => void {
    if (!this.config.privacy_toggles.enable_performance_tracking) {
      return () => {};
    }

    const widgetDetector = new DatadogWidgetDetector();
    let dashboardId = '';
    let dashboardLoadStart = 0;

    // Detect dashboard navigation
    const checkDashboardLoad = () => {
      const path = window.location.pathname;
      const dashboardMatch = path.match(/\/dashboard\/([^/]+)/);

      if (dashboardMatch && dashboardMatch[1] !== 'lists') {
        const id = dashboardMatch[1];

        // New dashboard load
        if (id !== dashboardId) {
          // Emit waterfall for previous dashboard if exists
          if (dashboardId && dashboardLoadStart) {
            const waterfall = performanceTracker.getWaterfall();
            if (waterfall) {
              onWaterfall(waterfall);
            }
          }

          // Reset and start new dashboard tracking
          dashboardId = id;
          dashboardLoadStart = performance.now();
          performanceTracker.reset();
          performanceTracker.startSpan('dashboard-load', {
            type: 'dashboard',
            assetId: id,
          });

          // Wait for widgets to load
          setTimeout(() => {
            performanceTracker.endSpan('dashboard-load');
          }, 2000);
        }
      }
    };

    // Observe widget loads
    const stopObserving = widgetDetector.observePanelLoads((widget: PanelDetectionResult) => {
      const widgetKey = `widget-${widget.id}`;

      performanceTracker.startSpan(widgetKey, {
        type: 'widget',
        assetId: widget.id,
        metadata: { name: widget.name || 'unknown' },
      });

      // Watch for widget render completion
      this.watchWidgetRender(widget.element, () => {
        performanceTracker.endSpan(widgetKey);
      });
    });

    // Initial check
    checkDashboardLoad();

    // Listen for navigation
    const originalPushState = window.history.pushState;
    window.history.pushState = function (...args) {
      originalPushState.apply(this, args);
      checkDashboardLoad();
    };

    const originalReplaceState = window.history.replaceState;
    window.history.replaceState = function (...args) {
      originalReplaceState.apply(this, args);
      checkDashboardLoad();
    };

    window.addEventListener('popstate', checkDashboardLoad);

    // Cleanup
    return () => {
      stopObserving();
      window.removeEventListener('popstate', checkDashboardLoad);
      window.history.pushState = originalPushState;
      window.history.replaceState = originalReplaceState;
    };
  }

  private watchWidgetRender(element: Element, onComplete: () => void): void {
    // Wait for widget to have content (loading spinner removed, data rendered)
    const checkRendered = () => {
      const spinner = element.querySelector('.loading, [data-testid="loading"]');
      const hasContent = element.querySelector('.widget-content, canvas, svg, table');

      if (!spinner && hasContent) {
        onComplete();
        return true;
      }
      return false;
    };

    // Immediate check
    if (checkRendered()) return;

    // Observe changes
    const observer = new MutationObserver(() => {
      if (checkRendered()) {
        observer.disconnect();
      }
    });

    observer.observe(element, { childList: true, subtree: true, attributes: true });

    // Timeout after 10s
    setTimeout(() => {
      onComplete();
      observer.disconnect();
    }, 10000);
  }
}
