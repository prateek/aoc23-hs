import { z } from 'zod';

// Base types
export const PlatformSchema = z.enum(['grafana', 'datadog']);
export type Platform = z.infer<typeof PlatformSchema>;

export const ActionSchema = z.enum([
  'view',
  'route_change',
  'time_range_changed',
  'panel_interaction',
  'search_used',
  'edit_opened',
  'share_opened',
  'alert_opened',
  'query_run',
  'variable_changed',
  'performance_waterfall',
]);
export type Action = z.infer<typeof ActionSchema>;

// Route schema
export const RouteSchema = z.object({
  url: z.string(),
  path: z.string(),
  query: z.record(z.union([z.string(), z.array(z.string())])),
});
export type Route = z.infer<typeof RouteSchema>;

// Identity schema
export const IdentitySchema = z.object({
  name: z.string().optional(),
  email: z.string().email().optional(),
  team: z.union([z.string(), z.array(z.string())]).optional(),
  source: z.enum(['options', 'dom', 'sso-config']),
});
export type Identity = z.infer<typeof IdentitySchema>;

// Asset schema
export const AssetTypeSchema = z.enum([
  'dashboard',
  'panel',
  'monitor',
  'alert',
  'explore',
  'logs',
]);
export type AssetType = z.infer<typeof AssetTypeSchema>;

export const AssetSchema = z.object({
  type: AssetTypeSchema,
  uid: z.string().optional(),
  name: z.string().optional(),
  panel_id: z.union([z.string(), z.number()]).optional(),
});
export type Asset = z.infer<typeof AssetSchema>;

// Template variable schema
export const TemplateVarSchema = z.object({
  key: z.string(),
  value_present: z.boolean(),
  value: z.string().optional(),
  redaction_applied: z.boolean().optional(),
});
export type TemplateVar = z.infer<typeof TemplateVarSchema>;

// Query metadata schema
export const QueryMetaSchema = z.object({
  data_source: z.string().optional(),
  text: z.string().optional(),
  tokens: z.array(z.string()).optional(),
  length: z.number(),
  redaction_applied: z.boolean(),
});
export type QueryMeta = z.infer<typeof QueryMetaSchema>;

// Time range schema
export const TimeBucketSchema = z.enum(['1m', '5m', '15m', '1h', '6h', '1d']);
export type TimeBucket = z.infer<typeof TimeBucketSchema>;

export const TimeRangeSchema = z.object({
  mode: z.enum(['relative', 'absolute']),
  from: z.string().optional(),
  to: z.string().optional(),
  bucket: TimeBucketSchema.optional(),
});
export type TimeRange = z.infer<typeof TimeRangeSchema>;

// Performance tracking schemas
export const PerformanceSpanSchema = z.object({
  name: z.string(), // e.g., "panel:cpu-usage", "template-var:region", "dashboard-load"
  type: z.enum([
    'dashboard',
    'panel',
    'template-var',
    'query',
    'data-fetch',
    'render',
    'widget',
    'other',
  ]),
  start_ms: z.number(), // milliseconds since navigation start
  duration_ms: z.number(),
  asset_id: z.string().optional(), // panel ID, var key, etc.
  metadata: z.record(z.union([z.string(), z.number(), z.boolean()])).optional(),
});
export type PerformanceSpan = z.infer<typeof PerformanceSpanSchema>;

export const PerformanceWaterfallSchema = z.object({
  total_duration_ms: z.number(),
  navigation_start: z.number(), // Unix timestamp ms
  spans: z.array(PerformanceSpanSchema),
  metrics: z
    .object({
      dom_content_loaded_ms: z.number().optional(),
      load_complete_ms: z.number().optional(),
      first_paint_ms: z.number().optional(),
      first_contentful_paint_ms: z.number().optional(),
      largest_contentful_paint_ms: z.number().optional(),
    })
    .optional(),
});
export type PerformanceWaterfall = z.infer<typeof PerformanceWaterfallSchema>;

// Base event schema
export const UXEventSchema = z.object({
  ts: z.string(),
  session_id: z.string().uuid(),
  anon_user_id: z.string().uuid(),
  platform: PlatformSchema,
  page_type: z.string(),
  action: ActionSchema,
  route: RouteSchema,
  ctx: z.record(z.union([z.string(), z.number(), z.boolean(), z.null()])).optional(),
});
export type UXEvent = z.infer<typeof UXEventSchema>;

// Extended event schema
export const UXEventV2Schema = UXEventSchema.extend({
  identity: IdentitySchema.optional(),
  asset: AssetSchema.optional(),
  template_vars: z.array(TemplateVarSchema).optional(),
  query: QueryMetaSchema.optional(),
  time_range: TimeRangeSchema.optional(),
  performance: PerformanceWaterfallSchema.optional(),
});
export type UXEventV2 = z.infer<typeof UXEventV2Schema>;

// Configuration schemas
export const TransportTypeSchema = z.enum(['generic_http', 'datadog_logs', 'grafana_loki']);
export type TransportType = z.infer<typeof TransportTypeSchema>;

export const RedactionPolicySchema = z.object({
  redact_credentials: z.boolean().default(true),
  redact_emails: z.boolean().default(true),
  redact_urls: z.boolean().default(true),
  redact_long_tokens: z.boolean().default(true),
  allowed_query_params: z.array(z.string()).default([]),
});
export type RedactionPolicy = z.infer<typeof RedactionPolicySchema>;

export const PrivacyTogglesSchema = z.object({
  collect_identity_options: z.boolean().default(false),
  dom_identity_scrape: z.boolean().default(false),
  collect_raw_asset_names: z.boolean().default(false),
  collect_template_var_values: z.boolean().default(false),
  collect_raw_queries: z.boolean().default(false),
  collect_absolute_timestamps: z.boolean().default(false),
  enable_time_bucketing: z.boolean().default(false),
  enable_performance_tracking: z.boolean().default(false),
});
export type PrivacyToggles = z.infer<typeof PrivacyTogglesSchema>;

export const ConfigSchema = z.object({
  endpoint_url: z.string().url().optional(),
  auth_token: z.string().optional(),
  transport_type: TransportTypeSchema.default('generic_http'),
  allowed_hosts: z.array(z.string()).default([
    'https://*.datadoghq.com/*',
    'https://*.datadoghq.eu/*',
    'https://*.ddog-gov.com/*',
    'https://*.grafana.net/*',
  ]),
  sampling_rate: z.number().min(0).max(100).default(20),
  privacy_toggles: PrivacyTogglesSchema.default({}),
  redaction_policy: RedactionPolicySchema.default({}),
  hash_salt: z.string(),
  debug: z.boolean().default(false),
  identity: IdentitySchema.optional(),
  idle_threshold_ms: z.number().default(60000),
  session_rotation_ms: z.number().default(86400000), // 24h
  time_bucket: TimeBucketSchema.optional(),
});
export type Config = z.infer<typeof ConfigSchema>;

// Event builders with privacy enforcement
export interface BuildContext {
  config: Config;
}

export function buildIdentity(ctx: BuildContext, source: Identity['source']): Identity | undefined {
  if (!ctx.config.privacy_toggles.collect_identity_options && source === 'options') {
    return undefined;
  }
  if (!ctx.config.privacy_toggles.dom_identity_scrape && source === 'dom') {
    return undefined;
  }

  const identity = ctx.config.identity;
  if (!identity) return undefined;

  return {
    ...identity,
    source,
  };
}

export function buildAsset(
  ctx: BuildContext,
  asset: Partial<Asset>
): Asset | undefined {
  if (!asset.type) return undefined;

  return {
    type: asset.type,
    uid: asset.uid,
    name: asset.name,
    panel_id: asset.panel_id,
  };
}

export function buildTemplateVars(
  ctx: BuildContext,
  vars: Record<string, string>
): TemplateVar[] | undefined {
  const entries = Object.entries(vars);
  if (entries.length === 0) return undefined;

  return entries.map(([key, value]) => {
    const result: TemplateVar = {
      key,
      value_present: !!value,
    };

    if (ctx.config.privacy_toggles.collect_template_var_values && value) {
      result.value = value;
    }

    return result;
  });
}

export function buildQueryMeta(
  ctx: BuildContext,
  query: string,
  dataSource?: string
): QueryMeta | undefined {
  if (!query) return undefined;

  const meta: QueryMeta = {
    length: query.length,
    redaction_applied: false,
  };

  if (dataSource) {
    meta.data_source = dataSource;
  }

  if (ctx.config.privacy_toggles.collect_raw_queries) {
    meta.text = query;
    // Tokenize: whitespace split, drop tokens >64 chars
    meta.tokens = query
      .split(/\s+/)
      .filter((t) => t.length > 0 && t.length <= 64);
  }

  return meta;
}

export function buildTimeRange(
  ctx: BuildContext,
  from: string,
  to: string,
  mode: TimeRange['mode']
): TimeRange | undefined {
  if (!from && !to) return undefined;

  const timeRange: TimeRange = { mode };

  if (ctx.config.privacy_toggles.collect_absolute_timestamps && mode === 'absolute') {
    timeRange.from = from;
    timeRange.to = to;
  }

  if (ctx.config.privacy_toggles.enable_time_bucketing && ctx.config.time_bucket) {
    timeRange.bucket = ctx.config.time_bucket;
  }

  return timeRange;
}
