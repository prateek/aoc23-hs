import { describe, it, expect } from 'vitest';
import {
  UXEventSchema,
  UXEventV2Schema,
  ConfigSchema,
  buildIdentity,
  buildTemplateVars,
  buildQueryMeta,
  buildTimeRange,
  type Config,
} from '../src/core/schema';

describe('Schema validation', () => {
  it('should validate base UXEvent', () => {
    const event = {
      ts: '2023-01-01T00:00:00Z',
      session_id: '550e8400-e29b-41d4-a716-446655440000',
      anon_user_id: '550e8400-e29b-41d4-a716-446655440001',
      platform: 'grafana',
      page_type: 'dashboard',
      action: 'view',
      route: {
        url: 'https://example.com',
        path: '/d/test',
        query: {},
      },
    };

    expect(() => UXEventSchema.parse(event)).not.toThrow();
  });

  it('should validate extended UXEventV2', () => {
    const event = {
      ts: '2023-01-01T00:00:00Z',
      session_id: '550e8400-e29b-41d4-a716-446655440000',
      anon_user_id: '550e8400-e29b-41d4-a716-446655440001',
      platform: 'datadog',
      page_type: 'logs',
      action: 'route_change',
      route: {
        url: 'https://example.com',
        path: '/logs',
        query: {},
      },
      identity: {
        name: 'Test User',
        email: 'test@example.com',
        source: 'options',
      },
      asset: {
        type: 'logs',
      },
    };

    expect(() => UXEventV2Schema.parse(event)).not.toThrow();
  });

  it('should validate Config', () => {
    const config = {
      endpoint_url: 'https://collector.example.com',
      auth_token: 'test-token',
      transport_type: 'generic_http',
      allowed_hosts: ['https://*.example.com/*'],
      sampling_rate: 50,
      privacy_toggles: {
        collect_identity_options: true,
        dom_identity_scrape: false,
        collect_raw_asset_names: false,
        collect_template_var_values: false,
        collect_raw_queries: false,
        collect_absolute_timestamps: false,
        enable_time_bucketing: false,
      },
      redaction_policy: {
        redact_credentials: true,
        redact_emails: true,
        redact_urls: true,
        redact_long_tokens: true,
        allowed_query_params: [],
      },
      hash_salt: 'test-salt',
      debug: false,
      idle_threshold_ms: 60000,
      session_rotation_ms: 86400000,
    };

    expect(() => ConfigSchema.parse(config)).not.toThrow();
  });

  it('should reject invalid sampling rate', () => {
    const config = {
      sampling_rate: 150, // Invalid: >100
      hash_salt: 'test',
    };

    expect(() => ConfigSchema.parse(config)).toThrow();
  });
});

describe('Event builders', () => {
  const mockConfig: Config = {
    privacy_toggles: {
      collect_identity_options: false,
      dom_identity_scrape: false,
      collect_raw_asset_names: false,
      collect_template_var_values: false,
      collect_raw_queries: false,
      collect_absolute_timestamps: false,
      enable_time_bucketing: false,
    },
    redaction_policy: {
      redact_credentials: true,
      redact_emails: true,
      redact_urls: true,
      redact_long_tokens: true,
      allowed_query_params: [],
    },
    hash_salt: 'test-salt',
  } as Config;

  it('buildIdentity should return undefined when collect_identity_options is false', () => {
    const result = buildIdentity({ config: mockConfig }, 'options');
    expect(result).toBeUndefined();
  });

  it('buildIdentity should return identity when enabled', () => {
    const config = {
      ...mockConfig,
      privacy_toggles: { ...mockConfig.privacy_toggles, collect_identity_options: true },
      identity: { name: 'Test', email: 'test@example.com', source: 'options' as const },
    };

    const result = buildIdentity({ config }, 'options');
    expect(result).toEqual({
      name: 'Test',
      email: 'test@example.com',
      source: 'options',
    });
  });

  it('buildTemplateVars should include keys only by default', () => {
    const vars = { region: 'us-east-1', env: 'prod' };
    const result = buildTemplateVars({ config: mockConfig }, vars);

    expect(result).toEqual([
      { key: 'region', value_present: true },
      { key: 'env', value_present: true },
    ]);
  });

  it('buildTemplateVars should include values when enabled', () => {
    const config = {
      ...mockConfig,
      privacy_toggles: { ...mockConfig.privacy_toggles, collect_template_var_values: true },
    };
    const vars = { region: 'us-east-1' };
    const result = buildTemplateVars({ config }, vars);

    expect(result).toEqual([{ key: 'region', value_present: true, value: 'us-east-1' }]);
  });

  it('buildQueryMeta should return metadata only by default', () => {
    const query = 'SELECT * FROM logs';
    const result = buildQueryMeta({ config: mockConfig }, query);

    expect(result).toEqual({
      length: query.length,
      redaction_applied: false,
    });
  });

  it('buildQueryMeta should include text when enabled', () => {
    const config = {
      ...mockConfig,
      privacy_toggles: { ...mockConfig.privacy_toggles, collect_raw_queries: true },
    };
    const query = 'SELECT * FROM logs';
    const result = buildQueryMeta({ config }, query);

    expect(result).toHaveProperty('text', query);
    expect(result).toHaveProperty('tokens');
  });

  it('buildTimeRange should return mode only by default', () => {
    const result = buildTimeRange({ config: mockConfig }, 'now-6h', 'now', 'relative');

    expect(result).toEqual({ mode: 'relative' });
  });

  it('buildTimeRange should include timestamps when enabled', () => {
    const config = {
      ...mockConfig,
      privacy_toggles: { ...mockConfig.privacy_toggles, collect_absolute_timestamps: true },
    };
    const result = buildTimeRange({ config }, '2023-01-01T00:00:00Z', '2023-01-01T06:00:00Z', 'absolute');

    expect(result).toEqual({
      mode: 'absolute',
      from: '2023-01-01T00:00:00Z',
      to: '2023-01-01T06:00:00Z',
    });
  });
});
