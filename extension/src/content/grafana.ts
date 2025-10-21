import type { UXEventV2, Config, Route, Asset } from '../core/schema';
import { parseURL, maybeHashAsset } from '../core/utils';
import { redactAndTruncateVar, redactAndTruncateQuery } from '../core/redactor';
import {
  PerformanceTracker,
  GrafanaPanelDetector,
  type PanelDetectionResult,
} from '../core/performance';

export interface GrafanaPageContext {
  pageType: string;
  asset?: Partial<Asset>;
  templateVars?: Record<string, string>;
  timeRange?: { from: string; to: string; mode: 'relative' | 'absolute' };
}

export class GrafanaTracker {
  constructor(private config: Config) {}

  // Detect page type and context from URL and DOM
  async detectPageContext(route: Route): Promise<GrafanaPageContext> {
    const path = route.path;

    // Dashboard: /d/:uid/:slug or /d-solo/:uid/:slug
    const dashboardMatch = path.match(/\/d(?:-solo)?\/([^/]+)(?:\/([^/]+))?/);
    if (dashboardMatch) {
      const uid = dashboardMatch[1];
      const asset: Partial<Asset> = {
        type: 'dashboard',
        uid: await maybeHashAsset(uid, this.config, true),
      };

      // Try to get dashboard name from DOM
      const titleEl = document.querySelector('[data-testid="dashboard-title"]');
      if (titleEl?.textContent) {
        asset.name = await maybeHashAsset(titleEl.textContent, this.config, true);
      }

      // Panel ID from solo mode
      if (path.includes('d-solo')) {
        const panelMatch = route.query['panelId'];
        if (panelMatch) {
          asset.panel_id = Array.isArray(panelMatch) ? panelMatch[0] : panelMatch;
        }
      }

      return {
        pageType: 'dashboard',
        asset,
        templateVars: this.extractTemplateVars(route),
        timeRange: this.extractTimeRange(route),
      };
    }

    // Explore: /explore
    if (path.includes('/explore')) {
      return {
        pageType: 'explore',
        asset: { type: 'explore' },
      };
    }

    // Alerting: /alerting/*
    if (path.includes('/alerting')) {
      return {
        pageType: 'alerting',
        asset: { type: 'alert' },
      };
    }

    // Default
    return {
      pageType: 'unknown',
    };
  }

  // Extract template variables from query params (var-*)
  extractTemplateVars(route: Route): Record<string, string> {
    const vars: Record<string, string> = {};
    Object.entries(route.query).forEach(([key, value]) => {
      if (key.startsWith('var-')) {
        const varKey = key.substring(4);
        vars[varKey] = Array.isArray(value) ? value[0] : value;
      }
    });
    return vars;
  }

  // Extract time range from query params
  extractTimeRange(route: Route): { from: string; to: string; mode: 'relative' | 'absolute' } | undefined {
    const from = route.query['from'];
    const to = route.query['to'];

    if (!from && !to) return undefined;

    const fromStr = Array.isArray(from) ? from[0] : from;
    const toStr = Array.isArray(to) ? to[0] : to;

    // Detect relative (e.g., now-6h) vs absolute (timestamp or ISO)
    const isRelative = fromStr?.includes('now') || toStr?.includes('now');

    return {
      from: fromStr || '',
      to: toStr || '',
      mode: isRelative ? 'relative' : 'absolute',
    };
  }

  // Build template vars for event with redaction
  async buildTemplateVarsForEvent(vars: Record<string, string>): Promise<Array<{ key: string; value_present: boolean; value?: string; redaction_applied?: boolean }> | undefined> {
    if (Object.keys(vars).length === 0) return undefined;

    const result = [];
    for (const [key, value] of Object.entries(vars)) {
      const item: { key: string; value_present: boolean; value?: string; redaction_applied?: boolean } = {
        key,
        value_present: !!value,
      };

      if (this.config.privacy_toggles.collect_template_var_values && value) {
        const redacted = redactAndTruncateVar(value, this.config.redaction_policy, 256);
        item.value = redacted.text;
        if (redacted.applied) {
          item.redaction_applied = true;
        }
      }

      result.push(item);
    }

    return result;
  }

  // Detect query execution (simplified - look for query editor or run query button)
  setupQueryDetection(onQuery: (query: string, dataSource?: string) => void): void {
    // Use MutationObserver to detect query runs
    // This is a simplified implementation; real detection would require deeper DOM inspection
    let lastQueryTime = 0;

    document.addEventListener('click', (e) => {
      const target = e.target as HTMLElement;
      // Look for "Run query" button or similar
      if (
        target.textContent?.includes('Run query') ||
        target.closest('[aria-label*="Run query"]') ||
        target.closest('[data-testid*="run-query"]')
      ) {
        const now = Date.now();
        // Throttle to max 1 query event per 5s
        if (now - lastQueryTime < 5000) return;
        lastQueryTime = now;

        // Try to extract query from editor (simplified)
        const queryEditor = document.querySelector('[data-testid="query-editor"]');
        const query = queryEditor?.textContent || '';
        onQuery(query);
      }
    }, true);
  }

  // Build query metadata for event with redaction
  async buildQueryMetaForEvent(query: string, dataSource?: string): Promise<{ data_source?: string; text?: string; tokens?: string[]; length: number; redaction_applied: boolean } | undefined> {
    if (!query) return undefined;

    const meta: { data_source?: string; text?: string; tokens?: string[]; length: number; redaction_applied: boolean } = {
      length: query.length,
      redaction_applied: false,
    };

    if (dataSource) {
      meta.data_source = await maybeHashAsset(dataSource, this.config, true);
    }

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

  // Setup performance tracking for dashboard loads
  setupPerformanceTracking(
    performanceTracker: PerformanceTracker,
    onWaterfall: (waterfall: any) => void
  ): () => void {
    if (!this.config.privacy_toggles.enable_performance_tracking) {
      return () => {};
    }

    const panelDetector = new GrafanaPanelDetector();
    let dashboardLoadStart = 0;
    let dashboardUid = '';
    const panelLoadStarts = new Map<string, number>();

    // Detect dashboard navigation
    const checkDashboardLoad = () => {
      const path = window.location.pathname;
      const dashboardMatch = path.match(/\/d(?:-solo)?\/([^/]+)/);

      if (dashboardMatch) {
        const uid = dashboardMatch[1];

        // New dashboard load
        if (uid !== dashboardUid) {
          // Emit waterfall for previous dashboard if exists
          if (dashboardUid && dashboardLoadStart) {
            const waterfall = performanceTracker.getWaterfall();
            if (waterfall) {
              onWaterfall(waterfall);
            }
          }

          // Reset and start new dashboard tracking
          dashboardUid = uid;
          dashboardLoadStart = performance.now();
          performanceTracker.reset();
          performanceTracker.startSpan('dashboard-load', {
            type: 'dashboard',
            assetId: uid,
          });

          // Track template variable resolution
          this.trackTemplateVarLoading(performanceTracker);

          // Wait for initial panels to load, then end dashboard span
          setTimeout(() => {
            performanceTracker.endSpan('dashboard-load');
          }, 2000); // Give panels 2s to appear
        }
      }
    };

    // Observe panel loads
    const stopObserving = panelDetector.observePanelLoads((panel: PanelDetectionResult) => {
      const panelKey = `panel-${panel.id}`;
      const startTime = panel.loadTime || performance.now();

      performanceTracker.startSpan(panelKey, {
        type: 'panel',
        assetId: panel.id,
        metadata: { name: panel.name || 'unknown' },
      });

      panelLoadStarts.set(panel.id, startTime);

      // Watch for panel render completion
      this.watchPanelRender(panel.element, () => {
        performanceTracker.endSpan(panelKey);
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

  private trackTemplateVarLoading(performanceTracker: PerformanceTracker): void {
    // Detect template variable dropdowns/inputs
    const varSelectors = document.querySelectorAll(
      '[data-testid^="variable-"], .template-variable'
    );

    varSelectors.forEach((varEl) => {
      const varKey =
        varEl.getAttribute('data-testid')?.replace('variable-', '') ||
        varEl.getAttribute('data-variable') ||
        'unknown';

      const spanName = `template-var:${varKey}`;
      performanceTracker.startSpan(spanName, {
        type: 'template-var',
        assetId: varKey,
      });

      // Watch for value to be loaded (simplistic: wait for content)
      const observer = new MutationObserver(() => {
        if (varEl.textContent && varEl.textContent.length > 0) {
          performanceTracker.endSpan(spanName);
          observer.disconnect();
        }
      });

      observer.observe(varEl, { childList: true, subtree: true, characterData: true });

      // Timeout after 5s
      setTimeout(() => {
        performanceTracker.endSpan(spanName);
        observer.disconnect();
      }, 5000);
    });
  }

  private watchPanelRender(element: Element, onComplete: () => void): void {
    // Wait for panel to have content (loading spinner removed, data rendered)
    const checkRendered = () => {
      const spinner = element.querySelector('.panel-loading, [data-testid="panel-loading"]');
      const hasContent = element.querySelector(
        '.panel-content, [data-testid="panel-content"], canvas, svg'
      );

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
