import type { Route } from './schema';
import { parseURL, filterQueryParams, type Config } from './utils';

export type RouteChangeHandler = (route: Route) => void;

export class SPARouter {
  private handler: RouteChangeHandler | null = null;
  private debounceTimer: number | null = null;
  private lastUrl: string = '';
  private config: Config | null = null;

  constructor(config: Config | null) {
    this.config = config;
  }

  // Start tracking route changes
  start(handler: RouteChangeHandler): void {
    this.handler = handler;
    this.lastUrl = window.location.href;

    // Wrap pushState and replaceState
    this.wrapHistoryMethod('pushState');
    this.wrapHistoryMethod('replaceState');

    // Listen to popstate
    window.addEventListener('popstate', this.handleRouteChange);

    // Listen to hashchange (for hash-based routing)
    window.addEventListener('hashchange', this.handleRouteChange);

    // Initial route
    this.handleRouteChange();
  }

  stop(): void {
    this.handler = null;
    window.removeEventListener('popstate', this.handleRouteChange);
    window.removeEventListener('hashchange', this.handleRouteChange);
  }

  updateConfig(config: Config): void {
    this.config = config;
  }

  private wrapHistoryMethod(method: 'pushState' | 'replaceState'): void {
    const original = window.history[method];
    const router = this;

    window.history[method] = function (...args) {
      const result = original.apply(this, args);
      router.handleRouteChange();
      return result;
    };
  }

  private handleRouteChange = (): void => {
    const currentUrl = window.location.href;
    if (currentUrl === this.lastUrl) return;

    this.lastUrl = currentUrl;
    this.debouncedEmit();
  };

  private debouncedEmit(): void {
    if (this.debounceTimer) {
      clearTimeout(this.debounceTimer);
    }

    this.debounceTimer = window.setTimeout(() => {
      this.emit();
    }, 300); // 300ms debounce
  }

  private emit(): void {
    if (!this.handler) return;

    const url = window.location.href;
    const parsed = parseURL(url);

    // Filter query params based on config
    const filteredQuery = this.config
      ? filterQueryParams(parsed.query, this.config.redaction_policy.allowed_query_params)
      : parsed.query;

    const route: Route = {
      url,
      path: parsed.path,
      query: filteredQuery,
    };

    this.handler(route);
  }

  // Manual trigger for DOM-based navigation detection
  triggerRouteCheck(): void {
    this.handleRouteChange();
  }
}
