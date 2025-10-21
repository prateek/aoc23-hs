import type { UXEventV2, Config, Route, Asset } from '../core/schema';
import { parseURL, maybeHashAsset } from '../core/utils';
import { redactAndTruncateVar, redactAndTruncateQuery } from '../core/redactor';

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
}
