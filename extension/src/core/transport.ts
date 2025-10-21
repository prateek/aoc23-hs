import type { UXEventV2, Config } from './schema';

export interface TransportAdapter {
  send(events: UXEventV2[], config: Config): Promise<void>;
}

// Exponential backoff retry
async function retry<T>(
  fn: () => Promise<T>,
  maxRetries = 3,
  baseDelay = 1000
): Promise<T> {
  let lastError: Error | null = null;

  for (let attempt = 0; attempt <= maxRetries; attempt++) {
    try {
      return await fn();
    } catch (error) {
      lastError = error as Error;
      if (attempt < maxRetries) {
        const delay = baseDelay * Math.pow(2, attempt) + Math.random() * 1000; // jitter
        const cappedDelay = Math.min(delay, 30000); // max 30s
        await new Promise((resolve) => setTimeout(resolve, cappedDelay));
      }
    }
  }

  throw lastError || new Error('Retry failed');
}

// Generic HTTP transport (NDJSON)
export class GenericHttpTransport implements TransportAdapter {
  async send(events: UXEventV2[], config: Config): Promise<void> {
    if (!config.endpoint_url) {
      throw new Error('endpoint_url not configured');
    }

    const ndjson = events.map((e) => JSON.stringify(e)).join('\n');

    await retry(async () => {
      const headers: HeadersInit = {
        'Content-Type': 'application/x-ndjson',
      };

      if (config.auth_token) {
        headers['Authorization'] = `Bearer ${config.auth_token}`;
      }

      const response = await fetch(config.endpoint_url!, {
        method: 'POST',
        headers,
        body: ndjson,
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
    });
  }
}

// Datadog Logs intake transport
export class DatadogLogsTransport implements TransportAdapter {
  async send(events: UXEventV2[], config: Config): Promise<void> {
    if (!config.endpoint_url) {
      throw new Error('endpoint_url not configured (e.g., https://http-intake.logs.datadoghq.com)');
    }
    if (!config.auth_token) {
      throw new Error('auth_token (DD-API-KEY) not configured');
    }

    // Datadog expects one event per line (NDJSON or array of JSON objects)
    // Using the v1 API: POST /v1/input
    const url = `${config.endpoint_url}/v1/input`;

    const payload = events.map((e) => ({
      ddsource: 'obs-ux-meter',
      ddtags: `platform:${e.platform},page_type:${e.page_type}`,
      hostname: 'browser-extension',
      message: JSON.stringify(e),
      service: 'obs-ux-meter',
      ...e,
    }));

    await retry(async () => {
      const response = await fetch(url, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'DD-API-KEY': config.auth_token!,
        },
        body: JSON.stringify(payload),
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
    });
  }
}

// Grafana Loki push transport
export class GrafanaLokiTransport implements TransportAdapter {
  async send(events: UXEventV2[], config: Config): Promise<void> {
    if (!config.endpoint_url) {
      throw new Error('endpoint_url not configured (e.g., https://logs-prod-us-central1.grafana.net)');
    }

    // Loki expects POST /loki/api/v1/push
    const url = `${config.endpoint_url}/loki/api/v1/push`;

    // Group events by platform for stream labels
    const streams: Record<string, { stream: Record<string, string>; values: [string, string][] }> = {};

    events.forEach((e) => {
      const streamKey = `${e.platform}:${e.page_type}`;
      if (!streams[streamKey]) {
        streams[streamKey] = {
          stream: {
            job: 'obs-ux-meter',
            platform: e.platform,
            page_type: e.page_type,
          },
          values: [],
        };
      }

      // Loki values: [[nanosecond_timestamp, log_line], ...]
      const timestamp = new Date(e.ts).getTime() * 1000000; // convert to nanoseconds
      streams[streamKey].values.push([timestamp.toString(), JSON.stringify(e)]);
    });

    const payload = {
      streams: Object.values(streams),
    };

    await retry(async () => {
      const headers: HeadersInit = {
        'Content-Type': 'application/json',
      };

      // Loki auth: Basic or Bearer
      if (config.auth_token) {
        headers['Authorization'] = `Bearer ${config.auth_token}`;
      }

      const response = await fetch(url, {
        method: 'POST',
        headers,
        body: JSON.stringify(payload),
      });

      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
    });
  }
}

// Factory
export function createTransport(config: Config): TransportAdapter {
  switch (config.transport_type) {
    case 'datadog_logs':
      return new DatadogLogsTransport();
    case 'grafana_loki':
      return new GrafanaLokiTransport();
    case 'generic_http':
    default:
      return new GenericHttpTransport();
  }
}
