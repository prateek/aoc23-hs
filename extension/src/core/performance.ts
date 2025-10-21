import type { PerformanceSpan, PerformanceWaterfall, Config } from './schema';

export interface SpanOptions {
  type: PerformanceSpan['type'];
  assetId?: string;
  metadata?: Record<string, string | number | boolean>;
}

export class PerformanceTracker {
  private navigationStart: number;
  private spans: Map<string, { start: number; options: SpanOptions }> = new Map();
  private completedSpans: PerformanceSpan[] = [];
  private enabled: boolean;

  constructor(private config: Config) {
    this.enabled = config.privacy_toggles.enable_performance_tracking;
    this.navigationStart = performance.now();

    if (this.enabled) {
      this.setupPerformanceObservers();
    }
  }

  // Start timing a span
  startSpan(name: string, options: SpanOptions): void {
    if (!this.enabled) return;

    const start = performance.now();
    this.spans.set(name, { start, options });
  }

  // End timing a span
  endSpan(name: string): void {
    if (!this.enabled) return;

    const spanData = this.spans.get(name);
    if (!spanData) return;

    const end = performance.now();
    const duration = end - spanData.start;

    const span: PerformanceSpan = {
      name,
      type: spanData.options.type,
      start_ms: Math.round(spanData.start),
      duration_ms: Math.round(duration),
      asset_id: spanData.options.assetId,
      metadata: spanData.options.metadata,
    };

    this.completedSpans.push(span);
    this.spans.delete(name);
  }

  // Measure a synchronous function
  measure<T>(name: string, options: SpanOptions, fn: () => T): T {
    this.startSpan(name, options);
    try {
      return fn();
    } finally {
      this.endSpan(name);
    }
  }

  // Measure an async function
  async measureAsync<T>(name: string, options: SpanOptions, fn: () => Promise<T>): Promise<T> {
    this.startSpan(name, options);
    try {
      return await fn();
    } finally {
      this.endSpan(name);
    }
  }

  // Mark a span with just start and duration (for observed events)
  markSpan(name: string, options: SpanOptions, startMs: number, durationMs: number): void {
    if (!this.enabled) return;

    const span: PerformanceSpan = {
      name,
      type: options.type,
      start_ms: Math.round(startMs),
      duration_ms: Math.round(durationMs),
      asset_id: options.assetId,
      metadata: options.metadata,
    };

    this.completedSpans.push(span);
  }

  // Get the complete waterfall
  getWaterfall(): PerformanceWaterfall | null {
    if (!this.enabled || this.completedSpans.length === 0) {
      return null;
    }

    // Calculate total duration (max end time of all spans)
    const maxEnd = Math.max(...this.completedSpans.map((s) => s.start_ms + s.duration_ms));

    // Collect browser performance metrics
    const metrics = this.getBrowserMetrics();

    return {
      total_duration_ms: Math.round(maxEnd),
      navigation_start: Date.now() - performance.now(),
      spans: [...this.completedSpans].sort((a, b) => a.start_ms - b.start_ms),
      metrics,
    };
  }

  // Reset the tracker (e.g., for new page load)
  reset(): void {
    this.navigationStart = performance.now();
    this.spans.clear();
    this.completedSpans = [];
  }

  private setupPerformanceObservers(): void {
    try {
      // Observe paint timing
      const paintObserver = new PerformanceObserver((list) => {
        for (const entry of list.getEntries()) {
          if (entry.entryType === 'paint') {
            this.markSpan(
              `browser:${entry.name}`,
              { type: 'render' },
              entry.startTime,
              entry.duration
            );
          }
        }
      });
      paintObserver.observe({ entryTypes: ['paint'] });

      // Observe LCP
      const lcpObserver = new PerformanceObserver((list) => {
        const entries = list.getEntries();
        const lastEntry = entries[entries.length - 1];
        if (lastEntry) {
          this.markSpan(
            'browser:largest-contentful-paint',
            { type: 'render' },
            lastEntry.startTime,
            lastEntry.duration || 0
          );
        }
      });
      lcpObserver.observe({ entryTypes: ['largest-contentful-paint'] });

      // Observe resource timing (for detecting data fetches)
      const resourceObserver = new PerformanceObserver((list) => {
        for (const entry of list.getEntries()) {
          if (entry.entryType === 'resource') {
            const resource = entry as PerformanceResourceTiming;
            // Only track XHR/fetch requests
            if (
              resource.initiatorType === 'xmlhttprequest' ||
              resource.initiatorType === 'fetch'
            ) {
              this.markSpan(
                `data-fetch:${this.truncateUrl(resource.name)}`,
                { type: 'data-fetch', metadata: { url: this.truncateUrl(resource.name) } },
                resource.startTime,
                resource.duration
              );
            }
          }
        }
      });
      resourceObserver.observe({ entryTypes: ['resource'] });
    } catch (error) {
      console.error('[obs-ux-meter] Failed to setup performance observers:', error);
    }
  }

  private getBrowserMetrics(): PerformanceWaterfall['metrics'] {
    try {
      const navigation = performance.getEntriesByType(
        'navigation'
      )[0] as PerformanceNavigationTiming;

      if (!navigation) return undefined;

      return {
        dom_content_loaded_ms: Math.round(navigation.domContentLoadedEventEnd),
        load_complete_ms: Math.round(navigation.loadEventEnd),
      };
    } catch {
      return undefined;
    }
  }

  private truncateUrl(url: string, maxLength = 100): string {
    if (url.length <= maxLength) return url;

    try {
      const parsed = new URL(url);
      return parsed.pathname + parsed.search;
    } catch {
      return url.substring(0, maxLength);
    }
  }
}

// Helper for DOM-based panel detection
export interface PanelDetector {
  detectPanels(): PanelDetectionResult[];
  observePanelLoads(callback: (panel: PanelDetectionResult) => void): () => void;
}

export interface PanelDetectionResult {
  id: string;
  name?: string;
  element: Element;
  loadTime?: number;
}

// Grafana panel detector
export class GrafanaPanelDetector implements PanelDetector {
  detectPanels(): PanelDetectionResult[] {
    const panels: PanelDetectionResult[] = [];

    // Grafana panels typically have data-panelid attribute
    const panelElements = document.querySelectorAll('[data-panelid], [data-panel-id], .panel-container');

    panelElements.forEach((element) => {
      const id =
        element.getAttribute('data-panelid') ||
        element.getAttribute('data-panel-id') ||
        `panel-${panels.length}`;

      // Try to get panel title
      const titleEl = element.querySelector('.panel-title, [data-testid="panel-title"]');
      const name = titleEl?.textContent?.trim();

      panels.push({ id, name, element });
    });

    return panels;
  }

  observePanelLoads(callback: (panel: PanelDetectionResult) => void): () => void {
    const observer = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        mutation.addedNodes.forEach((node) => {
          if (node instanceof Element) {
            // Check if the added node is a panel
            if (
              node.hasAttribute('data-panelid') ||
              node.hasAttribute('data-panel-id') ||
              node.classList.contains('panel-container')
            ) {
              const id =
                node.getAttribute('data-panelid') ||
                node.getAttribute('data-panel-id') ||
                `panel-${Date.now()}`;
              const titleEl = node.querySelector('.panel-title, [data-testid="panel-title"]');
              const name = titleEl?.textContent?.trim();
              callback({ id, name, element: node, loadTime: performance.now() });
            }

            // Also check descendants
            const panels = node.querySelectorAll(
              '[data-panelid], [data-panel-id], .panel-container'
            );
            panels.forEach((panel) => {
              const id =
                panel.getAttribute('data-panelid') ||
                panel.getAttribute('data-panel-id') ||
                `panel-${Date.now()}`;
              const titleEl = panel.querySelector('.panel-title, [data-testid="panel-title"]');
              const name = titleEl?.textContent?.trim();
              callback({ id, name, element: panel, loadTime: performance.now() });
            });
          }
        });
      });
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true,
    });

    return () => observer.disconnect();
  }
}

// Datadog widget detector
export class DatadogWidgetDetector implements PanelDetector {
  detectPanels(): PanelDetectionResult[] {
    const widgets: PanelDetectionResult[] = [];

    // Datadog widgets typically have data-widget-id or are .widget containers
    const widgetElements = document.querySelectorAll(
      '[data-widget-id], .widget, .dashboard-widget'
    );

    widgetElements.forEach((element) => {
      const id =
        element.getAttribute('data-widget-id') ||
        element.getAttribute('id') ||
        `widget-${widgets.length}`;

      // Try to get widget title
      const titleEl = element.querySelector('.widget-title, [data-testid="widget-title"]');
      const name = titleEl?.textContent?.trim();

      widgets.push({ id, name, element });
    });

    return widgets;
  }

  observePanelLoads(callback: (panel: PanelDetectionResult) => void): () => void {
    const observer = new MutationObserver((mutations) => {
      mutations.forEach((mutation) => {
        mutation.addedNodes.forEach((node) => {
          if (node instanceof Element) {
            if (
              node.hasAttribute('data-widget-id') ||
              node.classList.contains('widget') ||
              node.classList.contains('dashboard-widget')
            ) {
              const id =
                node.getAttribute('data-widget-id') ||
                node.getAttribute('id') ||
                `widget-${Date.now()}`;
              const titleEl = node.querySelector('.widget-title, [data-testid="widget-title"]');
              const name = titleEl?.textContent?.trim();
              callback({ id, name, element: node, loadTime: performance.now() });
            }

            // Check descendants
            const widgets = node.querySelectorAll(
              '[data-widget-id], .widget, .dashboard-widget'
            );
            widgets.forEach((widget) => {
              const id =
                widget.getAttribute('data-widget-id') ||
                widget.getAttribute('id') ||
                `widget-${Date.now()}`;
              const titleEl = widget.querySelector('.widget-title, [data-testid="widget-title"]');
              const name = titleEl?.textContent?.trim();
              callback({ id, name, element: widget, loadTime: performance.now() });
            });
          }
        });
      });
    });

    observer.observe(document.body, {
      childList: true,
      subtree: true,
    });

    return () => observer.disconnect();
  }
}
