import type { UXEventV2, Route, Platform } from '../core/schema';
import { buildIdentity, buildAsset, buildTemplateVars, buildTimeRange } from '../core/schema';
import {
  getConfig,
  getOrCreateAnonUserId,
  getOrCreateSessionId,
  updateLastActivity,
  shouldSample,
  debug,
  matchHost,
} from '../core/utils';
import { SPARouter } from '../core/router';
import { IdleTracker } from '../core/idle';
import { EventQueue } from '../core/queue';
import { createTransport } from '../core/transport';
import { GrafanaTracker } from './grafana';
import { DatadogTracker } from './datadog';

// Main tracker class
class ObsUXMeter {
  private config = null as any;
  private anonUserId = '';
  private sessionId = '';
  private platform: Platform | null = null;
  private router: SPARouter | null = null;
  private idleTracker: IdleTracker | null = null;
  private queue: EventQueue | null = null;
  private grafanaTracker: GrafanaTracker | null = null;
  private datadogTracker: DatadogTracker | null = null;
  private sampled = false;

  async init(): Promise<void> {
    try {
      // Load config
      this.config = await getConfig();
      if (!this.config) {
        debug(this.config, 'No config found, skipping initialization');
        return;
      }

      // Check if current host is allowed
      if (!matchHost(window.location.href, this.config.allowed_hosts)) {
        debug(this.config, 'Current host not in allowlist');
        return;
      }

      // Detect platform
      this.platform = this.detectPlatform();
      if (!this.platform) {
        debug(this.config, 'Could not detect platform');
        return;
      }

      // Get/create IDs
      this.anonUserId = await getOrCreateAnonUserId();
      this.sessionId = await getOrCreateSessionId(this.config);

      // Check sampling
      this.sampled = shouldSample(this.anonUserId, this.config.sampling_rate);
      if (!this.sampled) {
        debug(this.config, 'User not sampled, skipping tracking');
        return;
      }

      debug(this.config, 'Initializing tracker for platform:', this.platform);

      // Initialize queue
      this.queue = new EventQueue({
        maxBatchSize: 30,
        maxBatchInterval: 10000,
      });

      await this.queue.init(async (events) => {
        const transport = createTransport(this.config);
        await transport.send(events, this.config);
      });

      // Initialize platform tracker
      if (this.platform === 'grafana') {
        this.grafanaTracker = new GrafanaTracker(this.config);
      } else if (this.platform === 'datadog') {
        this.datadogTracker = new DatadogTracker(this.config);
      }

      // Initialize router
      this.router = new SPARouter(this.config);
      this.router.start((route) => this.handleRouteChange(route));

      // Initialize idle tracker
      this.idleTracker = new IdleTracker(
        this.config.idle_threshold_ms,
        15000 // heartbeat every 15s
      );
      this.idleTracker.start(
        (isIdle) => this.handleIdleStateChange(isIdle),
        () => this.handleHeartbeat()
      );

      // Setup query detection
      if (this.grafanaTracker) {
        this.grafanaTracker.setupQueryDetection((query, dataSource) =>
          this.handleQuery(query, dataSource)
        );
      } else if (this.datadogTracker) {
        this.datadogTracker.setupQueryDetection((query) => this.handleQuery(query));
      }

      // Setup unload handler
      window.addEventListener('beforeunload', () => this.handleUnload());

      debug(this.config, 'Tracker initialized successfully');
    } catch (error) {
      console.error('[obs-ux-meter] Initialization failed:', error);
    }
  }

  private detectPlatform(): Platform | null {
    const host = window.location.host;
    const path = window.location.pathname;

    if (
      host.includes('datadoghq.com') ||
      host.includes('datadoghq.eu') ||
      host.includes('ddog-gov.com')
    ) {
      return 'datadog';
    }

    if (host.includes('grafana.net') || path.includes('/grafana/')) {
      return 'grafana';
    }

    return null;
  }

  private async handleRouteChange(route: Route): Promise<void> {
    try {
      await updateLastActivity();

      let pageType = 'unknown';
      let asset = undefined;
      let templateVars = undefined;
      let timeRange = undefined;

      if (this.platform === 'grafana' && this.grafanaTracker) {
        const context = await this.grafanaTracker.detectPageContext(route);
        pageType = context.pageType;
        asset = context.asset ? buildAsset({ config: this.config }, context.asset) : undefined;
        templateVars = context.templateVars
          ? await this.grafanaTracker.buildTemplateVarsForEvent(context.templateVars)
          : undefined;
        if (context.timeRange) {
          timeRange = buildTimeRange(
            { config: this.config },
            context.timeRange.from,
            context.timeRange.to,
            context.timeRange.mode
          );
        }
      } else if (this.platform === 'datadog' && this.datadogTracker) {
        const context = await this.datadogTracker.detectPageContext(route);
        pageType = context.pageType;
        asset = context.asset ? buildAsset({ config: this.config }, context.asset) : undefined;
      }

      const event: UXEventV2 = {
        ts: new Date().toISOString(),
        session_id: this.sessionId,
        anon_user_id: this.anonUserId,
        platform: this.platform!,
        page_type: pageType,
        action: 'route_change',
        route,
        identity: buildIdentity({ config: this.config }, 'options'),
        asset,
        template_vars: templateVars,
        time_range: timeRange,
      };

      await this.queue?.enqueue(event);
      debug(this.config, 'Route change event:', event);
    } catch (error) {
      console.error('[obs-ux-meter] Route change handler failed:', error);
    }
  }

  private handleIdleStateChange(isIdle: boolean): void {
    debug(this.config, 'Idle state changed:', isIdle);
  }

  private async handleHeartbeat(): Promise<void> {
    await this.queue?.flush();
  }

  private async handleQuery(query: string, dataSource?: string): Promise<void> {
    try {
      await updateLastActivity();

      let queryMeta = undefined;

      if (this.platform === 'grafana' && this.grafanaTracker) {
        queryMeta = await this.grafanaTracker.buildQueryMetaForEvent(query, dataSource);
      } else if (this.platform === 'datadog' && this.datadogTracker) {
        queryMeta = await this.datadogTracker.buildQueryMetaForEvent(query);
      }

      if (!queryMeta) return;

      const event: UXEventV2 = {
        ts: new Date().toISOString(),
        session_id: this.sessionId,
        anon_user_id: this.anonUserId,
        platform: this.platform!,
        page_type: 'query',
        action: 'query_run',
        route: {
          url: window.location.href,
          path: window.location.pathname,
          query: {},
        },
        query: queryMeta,
      };

      await this.queue?.enqueue(event);
      debug(this.config, 'Query event:', event);
    } catch (error) {
      console.error('[obs-ux-meter] Query handler failed:', error);
    }
  }

  private handleUnload(): void {
    // Use sendBeacon for best-effort delivery on unload
    if (this.queue && this.config?.endpoint_url) {
      this.queue.flushWithBeacon(this.config.endpoint_url, this.config.auth_token);
    }
  }
}

// Initialize tracker
const tracker = new ObsUXMeter();
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', () => tracker.init());
} else {
  tracker.init();
}
