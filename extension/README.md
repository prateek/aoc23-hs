# Observability UX Meter

**Privacy-preserving Chrome Manifest V3 extension for tracking high-signal usage metadata in Grafana and Datadog UIs.**

Built for migration analysis and UX research across internal teams with strict privacy defaults, configurable collection, robust delivery, and comprehensive testing.

---

## Features

- **Privacy-first by default**: Captures metadata only (page types, routes, interactions), no sensitive content
- **Strict redaction engine**: Removes credentials, secrets, PII with configurable policies
- **Reliable delivery**: Batching, exponential backoff retry, IndexedDB offline buffer, `sendBeacon` on unload
- **Multiple transports**: Generic HTTP (NDJSON), Datadog Logs API, Grafana Loki
- **Configurable privacy toggles**: Opt-in for identity, asset names, template variables, queries, timestamps
- **Deterministic sampling**: Consistent per-user sampling across sessions
- **Comprehensive testing**: Vitest unit tests + Playwright E2E tests

---

## Quick Start

### 1. Install Dependencies

```bash
cd extension
npm install
```

### 2. Build the Extension

```bash
npm run build
```

This creates a `dist/` directory with the compiled extension.

### 3. Load Unpacked Extension in Chrome

1. Open Chrome and navigate to `chrome://extensions/`
2. Enable **Developer mode** (top right toggle)
3. Click **Load unpacked**
4. Select the `extension/dist/` directory

### 4. Configure Settings

1. Click the extension icon or go to `chrome://extensions/`
2. Click **Details** → **Extension options**
3. Configure:
   - **Endpoint URL**: Your collector endpoint (e.g., `http://localhost:8787`)
   - **Transport Type**: `generic_http`, `datadog_logs`, or `grafana_loki`
   - **Sampling Rate**: 0-100%
   - **Privacy Toggles**: All OFF by default (see Privacy section)
4. Click **Save Settings**

### 5. Test

Visit a Grafana or Datadog instance (or use mock sites in `tools/mock-sites/`) and navigate dashboards. Events will be sent to your configured collector.

---

## Privacy Summary

### What is Collected by Default (Privacy-Preserving)

✅ **Metadata only:**
- Platform (Grafana/Datadog)
- Page type (dashboard/logs/monitors/explore/etc.)
- Action (view, route_change, time_range_changed, etc.)
- URL path (e.g., `/d/:uid/:slug`)
- Query param **keys** only (values stripped unless allowlisted)
- Asset UIDs and names are **hashed** by default (SHA-256 + local salt)
- Template variable **keys** only (values not collected)
- Query **length** and presence (text not collected)
- Time range **mode** (relative/absolute, no timestamps)

✅ **Session & user tracking:**
- Anonymous user ID (stable UUIDv4 in local storage)
- Session ID (rotates daily or after 30min idle)
- Timestamps (event time only)

❌ **Never Collected:**
- DOM `innerText` from input fields, textareas, `[contenteditable]`, password fields
- Network request/response payloads
- Cookies or tokens from storage
- Free-text content unless explicitly enabled

### What is Collected with Enhanced Toggles (Opt-In)

When privacy toggles are **enabled**, the following may be collected **with redaction**:

- **Identity** (name, email, team) from Options page or DOM scraping
- **Raw asset names/UIDs** (instead of hashes)
- **Template variable values** (truncated to 256 chars, redacted)
- **Query text** (truncated to 500 chars, redacted, tokenized)
- **Absolute timestamps** for time ranges

All enhanced captures apply the **redaction engine** to strip credentials, secrets, and PII.

---

## Redaction Policy

The redaction engine automatically removes:

- **AWS keys**: `AKIA...`, `ASIA...`
- **Google API keys**: `AIza...`
- **JWT tokens**: `eyJ...`
- **Long hex strings**: 32+ characters (likely tokens)
- **Bearer tokens**: `Bearer <token>`
- **Secret assignments**: `password=...`, `api_key:...`, `token=...`
- **RSA private keys**: `-----BEGIN RSA PRIVATE KEY-----`
- **Emails**: All except identity email → `***@***`
- **Long tokens**: >128 chars → `<LONG_TOKEN>`
- **URLs**: Query strings stripped (unless params are allowlisted)

### Testing Redaction

Use the **Options page** to test redaction:

1. Go to extension options
2. Scroll to **Redaction Policy**
3. Configure allowed query params (e.g., `from,to`)
4. Click **Send Test Event** to verify

---

## Configuration Reference

### Transport Settings

| Field | Description | Example |
|-------|-------------|---------|
| `endpoint_url` | HTTP endpoint for events | `https://collector.example.com` |
| `auth_token` | Bearer token or API key | `abc123...` (stored securely) |
| `transport_type` | Adapter type | `generic_http`, `datadog_logs`, `grafana_loki` |

### Privacy Toggles (All OFF by default)

| Toggle | What it enables |
|--------|----------------|
| `collect_identity_options` | Capture name/email/team from Options |
| `dom_identity_scrape` | Extract identity from DOM (experimental) |
| `collect_raw_asset_names` | Store dashboard/monitor names as plaintext (else hashed) |
| `collect_template_var_values` | Include template variable values (redacted, truncated to 256 chars) |
| `collect_raw_queries` | Include query text (redacted, truncated to 500 chars) |
| `collect_absolute_timestamps` | Capture `from`/`to` timestamps for time ranges |
| `enable_time_bucketing` | Round time ranges to nearest bucket (1m/5m/15m/1h/6h/1d) |

### Redaction Policy

| Setting | Default | Description |
|---------|---------|-------------|
| `redact_credentials` | ✅ | Remove AWS keys, JWTs, Bearer tokens, secrets |
| `redact_emails` | ✅ | Mask emails except identity email |
| `redact_urls` | ✅ | Strip query strings from URLs |
| `redact_long_tokens` | ✅ | Replace tokens >128 chars with `<LONG_TOKEN>` |
| `allowed_query_params` | `[]` | Allowlist of query params to preserve (e.g., `['from', 'to']`) |

### Other Settings

| Field | Default | Description |
|-------|---------|-------------|
| `sampling_rate` | `20` | Percentage of users to track (0-100) |
| `allowed_hosts` | See defaults | Host patterns where extension runs (glob syntax) |
| `debug` | `false` | Enable `console.debug` logging |
| `idle_threshold_ms` | `60000` | Idle timeout (60s) |
| `session_rotation_ms` | `86400000` | Session rotation interval (24h) |

---

## Transport Adapters

### 1. Generic HTTP (NDJSON)

**Endpoint format:** Any HTTP endpoint accepting `POST` with NDJSON body.

```bash
# Example collector
curl -X POST http://localhost:8787 \
  -H "Content-Type: application/x-ndjson" \
  -H "Authorization: Bearer YOUR_TOKEN" \
  -d $'{"ts":"2023-01-01T00:00:00Z","platform":"grafana","action":"view"}\n{"ts":"2023-01-01T00:01:00Z","platform":"datadog","action":"route_change"}'
```

**Config:**
- `endpoint_url`: `http://your-collector.example.com`
- `auth_token`: Optional Bearer token
- `transport_type`: `generic_http`

### 2. Datadog Logs

**Endpoint format:** Datadog Logs intake API.

```bash
curl -X POST https://http-intake.logs.datadoghq.com/v1/input \
  -H "Content-Type: application/json" \
  -H "DD-API-KEY: YOUR_DD_API_KEY" \
  -d '[{"ddsource":"obs-ux-meter","service":"obs-ux-meter","message":"..."}]'
```

**Config:**
- `endpoint_url`: `https://http-intake.logs.datadoghq.com` (US) or `https://http-intake.logs.datadoghq.eu` (EU)
- `auth_token`: Your Datadog API key
- `transport_type`: `datadog_logs`

### 3. Grafana Loki

**Endpoint format:** Loki push API.

```bash
curl -X POST https://logs-prod-us-central1.grafana.net/loki/api/v1/push \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer YOUR_LOKI_TOKEN" \
  -d '{"streams":[{"stream":{"job":"obs-ux-meter","platform":"grafana"},"values":[["1672531200000000000","{...}"]]}]}'
```

**Config:**
- `endpoint_url`: `https://logs-prod-us-central1.grafana.net` (or your Loki instance)
- `auth_token`: Loki API token
- `transport_type`: `grafana_loki`

---

## Domains & Activation

The extension only runs on **allowed hosts** configured in Options. Default patterns:

```
https://*.datadoghq.com/*
https://*.datadoghq.eu/*
https://*.ddog-gov.com/*
https://*.grafana.net/*
https://*/grafana/*  (for self-hosted Grafana)
```

You can add custom patterns in the **Allowed Host Patterns** field (one per line).

---

## Event Schema

### Base Event (`UXEvent`)

```typescript
{
  ts: "2023-01-01T00:00:00Z",
  session_id: "550e8400-e29b-41d4-a716-446655440000",
  anon_user_id: "550e8400-e29b-41d4-a716-446655440001",
  platform: "grafana" | "datadog",
  page_type: "dashboard" | "logs" | "monitors" | "explore" | ...,
  action: "view" | "route_change" | "query_run" | ...,
  route: {
    url: "https://grafana.net/d/abc123/dashboard",
    path: "/d/abc123/dashboard",
    query: { "var-region": "", "from": "now-6h" }  // Values stripped unless allowlisted
  },
  ctx?: { /* custom context */ }
}
```

### Extended Event (`UXEventV2`)

Includes optional fields when privacy toggles are enabled:

```typescript
{
  ...baseEvent,
  identity?: {
    name: "Jane Doe",
    email: "jane@example.com",
    team: "Platform",
    source: "options" | "dom" | "sso-config"
  },
  asset?: {
    type: "dashboard" | "monitor" | ...,
    uid: "abc123" | "sha256-hash",  // Hashed by default
    name: "Production Metrics" | "sha256-hash",
    panel_id: "2"
  },
  template_vars?: [
    { key: "region", value_present: true, value: "us-east-1", redaction_applied: false }
  ],
  query?: {
    data_source: "prometheus",
    text: "SELECT * FROM logs WHERE...",  // Redacted, truncated
    tokens: ["SELECT", "*", "FROM", "logs"],
    length: 1024,
    redaction_applied: true
  },
  time_range?: {
    mode: "relative" | "absolute",
    from: "2023-01-01T00:00:00Z",  // Only if collect_absolute_timestamps ON
    to: "2023-01-01T06:00:00Z",
    bucket: "1h"  // If time_bucketing ON
  }
}
```

---

## Performance Waterfall Tracking

### Overview

The extension includes **opt-in waterfall-style performance tracking** to measure dashboard and panel load times. This provides detailed timing breakdowns for:

- **Dashboard load**: Total time from navigation to full render
- **Panel loads**: Individual panel render times
- **Template variables**: Variable resolution timing
- **Data fetches**: XHR/fetch request timing
- **Browser metrics**: DOM content loaded, LCP, paint timing

### Enabling Performance Tracking

**Privacy Default**: Performance tracking is **OFF** by default.

To enable:
1. Go to extension Options
2. Scroll to **Privacy Toggles**
3. Check **"Enable performance waterfall tracking"**
4. Save settings

### Waterfall Event Structure

When a dashboard finishes loading, a `performance_waterfall` event is emitted:

```json
{
  "ts": "2023-01-01T00:05:23Z",
  "session_id": "...",
  "anon_user_id": "...",
  "platform": "grafana",
  "page_type": "performance",
  "action": "performance_waterfall",
  "route": {
    "url": "https://grafana.net/d/abc123/prod-metrics",
    "path": "/d/abc123/prod-metrics",
    "query": {}
  },
  "performance": {
    "total_duration_ms": 2847,
    "navigation_start": 1672531523000,
    "spans": [
      {
        "name": "dashboard-load",
        "type": "dashboard",
        "start_ms": 0,
        "duration_ms": 2000,
        "asset_id": "abc123"
      },
      {
        "name": "template-var:region",
        "type": "template-var",
        "start_ms": 120,
        "duration_ms": 340,
        "asset_id": "region"
      },
      {
        "name": "panel-2",
        "type": "panel",
        "start_ms": 450,
        "duration_ms": 1200,
        "asset_id": "2",
        "metadata": {
          "name": "CPU Usage"
        }
      },
      {
        "name": "panel-4",
        "type": "panel",
        "start_ms": 480,
        "duration_ms": 2367,
        "asset_id": "4",
        "metadata": {
          "name": "Memory Usage"
        }
      },
      {
        "name": "data-fetch:/api/datasources/proxy/5/api/v1/query_range",
        "type": "data-fetch",
        "start_ms": 500,
        "duration_ms": 890,
        "metadata": {
          "url": "/api/datasources/proxy/5/api/v1/query_range"
        }
      },
      {
        "name": "browser:first-paint",
        "type": "render",
        "start_ms": 234,
        "duration_ms": 0
      },
      {
        "name": "browser:first-contentful-paint",
        "type": "render",
        "start_ms": 421,
        "duration_ms": 0
      }
    ],
    "metrics": {
      "dom_content_loaded_ms": 1234,
      "load_complete_ms": 2500
    }
  }
}
```

### Span Types

| Type | Description | Example |
|------|-------------|---------|
| `dashboard` | Overall dashboard load time | Dashboard from nav start to all panels rendered |
| `panel` | Individual panel render time | "CPU Usage" panel load |
| `template-var` | Template variable resolution | "region" dropdown populated |
| `query` | Query execution timing | Prometheus query run |
| `data-fetch` | XHR/fetch request timing | API call to fetch metrics |
| `render` | Browser rendering milestones | First paint, LCP |
| `widget` | Datadog widget load (Datadog-specific) | Widget render time |
| `other` | Miscellaneous timing spans | Custom marks |

### How It Works

#### Grafana

1. **Dashboard detection**: Monitors URL for `/d/:uid` pattern
2. **Panel detection**: Uses `MutationObserver` to detect panel DOM elements (`[data-panelid]`)
3. **Template vars**: Detects `[data-testid^="variable-"]` elements, measures time to populate
4. **Panel render**: Watches for loading spinners to disappear and content (canvas/SVG) to appear
5. **Waterfall emission**: After 2 seconds of no new panels, emits complete waterfall

#### Datadog

1. **Dashboard detection**: Monitors URL for `/dashboard/:id`
2. **Widget detection**: Detects `[data-widget-id], .widget` elements
3. **Widget render**: Watches for loading indicators to disappear
4. **Waterfall emission**: After 2 seconds, emits complete waterfall

### Use Cases

**Migration Planning:**
```sql
-- Compare Grafana vs Datadog dashboard performance
SELECT
  platform,
  AVG(performance->>'total_duration_ms'::text::int) as avg_load_ms,
  PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY (performance->>'total_duration_ms')::int) as p95_load_ms
FROM ux_events
WHERE action = 'performance_waterfall'
GROUP BY platform;
```

**Slow Panel Detection:**
```sql
-- Find panels that take >5 seconds to load
SELECT
  span->>'name' as panel_name,
  AVG((span->>'duration_ms')::int) as avg_duration_ms
FROM ux_events,
  jsonb_array_elements(performance->'spans') as span
WHERE action = 'performance_waterfall'
  AND span->>'type' = 'panel'
  AND (span->>'duration_ms')::int > 5000
GROUP BY span->>'name'
ORDER BY avg_duration_ms DESC;
```

**Template Variable Performance:**
```sql
-- Average time to resolve template variables
SELECT
  span->>'asset_id' as var_name,
  AVG((span->>'duration_ms')::int) as avg_resolution_ms
FROM ux_events,
  jsonb_array_elements(performance->'spans') as span
WHERE span->>'type' = 'template-var'
GROUP BY var_name
ORDER BY avg_resolution_ms DESC;
```

**Dashboard Load Timeline Visualization:**
```python
import pandas as pd
import plotly.express as px

# Load waterfall data
df = pd.read_json('waterfall_events.ndjson', lines=True)

# Extract spans
spans = []
for idx, row in df.iterrows():
    for span in row['performance']['spans']:
        spans.append({
            'dashboard': row['route']['path'],
            'name': span['name'],
            'type': span['type'],
            'start': span['start_ms'],
            'end': span['start_ms'] + span['duration_ms'],
            'duration': span['duration_ms']
        })

spans_df = pd.DataFrame(spans)

# Create Gantt chart
fig = px.timeline(
    spans_df,
    x_start='start',
    x_end='end',
    y='name',
    color='type',
    title='Dashboard Load Waterfall'
)
fig.show()
```

### Privacy Considerations

**What is collected:**
- Panel IDs (hashed by default unless `collect_raw_asset_names` is ON)
- Panel names (hashed)
- Timing durations (milliseconds)
- Span types and metadata

**What is NOT collected:**
- Panel data/query results
- User interactions within panels
- DOM content or innerText
- Credentials or tokens

**Sampling:**
- Performance waterfalls respect the global `sampling_rate`
- Heavy overhead? Reduce sampling or disable performance tracking

---

## Development

### Project Structure

```
extension/
├── src/
│   ├── content/          # Platform-specific trackers
│   │   ├── grafana.ts
│   │   ├── datadog.ts
│   │   └── inject.ts     # Main entry point
│   ├── core/             # Core utilities
│   │   ├── schema.ts     # Zod schemas & types
│   │   ├── utils.ts      # UUID, hashing, storage
│   │   ├── redactor.ts   # Redaction engine
│   │   ├── router.ts     # SPA navigation tracking
│   │   ├── idle.ts       # Idle/active detection
│   │   ├── queue.ts      # Batching & IndexedDB
│   │   ├── transport.ts  # HTTP adapters
│   │   └── performance.ts # Waterfall tracking
│   ├── background/
│   │   └── worker.ts     # Service worker
│   └── options/
│       ├── index.html
│       └── options.tsx   # Preact UI
├── test/                 # Unit tests (Vitest)
├── tests-integration/    # E2E tests (Playwright)
├── tools/
│   ├── mock-collector.js
│   └── mock-sites/       # Test HTML pages
├── manifest.json
├── package.json
├── vite.config.ts
└── README.md
```

### Build & Dev Commands

```bash
# Install dependencies
npm install

# Build for production
npm run build

# Watch mode (rebuild on changes)
npm run dev

# Run unit tests
npm test

# Run unit tests in watch mode
npm run test:watch

# Run integration tests (Playwright)
npm run test:integration

# Run E2E tests with UI (debug)
npm run e2e:headed

# Lint & format
npm run lint
npm run format

# Type check
npm run typecheck
```

### Running Tests

#### Unit Tests (Vitest)

```bash
npm test
```

Tests in `test/`:
- `router.test.ts` - URL change detection, debouncing
- `idle.test.ts` - Idle/active transitions, heartbeat
- `queue.test.ts` - Batching, retry, IndexedDB persistence
- `schema.test.ts` - Zod validation, privacy enforcement
- `redactor.test.ts` - All redaction patterns (AWS keys, JWTs, emails, etc.)

#### Integration Tests (Playwright)

```bash
# Build first
npm run build

# Run all E2E tests
npm run test:integration

# Run with Playwright UI (debug)
npm run e2e:headed
```

Tests in `tests-integration/`:
- `route.spec.ts` - SPA navigation, hashed asset UIDs
- `identity.spec.ts` - Identity capture (options, DOM scrape)
- `template-vars.spec.ts` - Keys vs values, redaction
- `queries.spec.ts` - Metadata vs enhanced text, throttling
- `time-range.spec.ts` - Relative/absolute, bucketing
- `transport.spec.ts` - Retry, offline buffer
- `adapters.spec.ts` - Datadog/Loki payload formats
- `sampling.spec.ts` - Deterministic sampling
- `privacy-guard.spec.ts` - Secret injection, no leakage

Tests use:
- `tools/mock-collector.js` - NDJSON HTTP server on port 8787
- `tools/mock-sites/grafana.html` - Simulated Grafana dashboard
- `tools/mock-sites/datadog.html` - Simulated Datadog logs

---

## Troubleshooting

### Extension Not Activating

**Issue**: Extension doesn't run on my Grafana instance.

**Solution**:
1. Check **Allowed Host Patterns** in Options
2. Add your host pattern (e.g., `https://grafana.mycompany.com/*`)
3. Reload the extension and refresh the page

### Events Not Sending

**Issue**: No events appearing in collector.

**Solution**:
1. Check **Debug** toggle in Options (enables console logging)
2. Open DevTools → Console, look for `[obs-ux-meter]` messages
3. Verify:
   - `endpoint_url` is correct and reachable
   - Sampling rate > 0
   - Current user is sampled (logged at initialization)
4. Check Network tab for POST requests to your endpoint

### CSP Errors

**Issue**: Content Security Policy errors in console.

**Solution**:
- Ensure `manifest.json` CSP allows extension scripts
- Check target site's CSP doesn't block extension injection

### Permissions Errors

**Issue**: Extension can't access storage or make requests.

**Solution**:
- Verify `manifest.json` has `storage` and `alarms` permissions
- Ensure `host_permissions` match your target sites

### IndexedDB Quota Exceeded

**Issue**: Too many events queued offline.

**Solution**:
- Events flush every 30 events or 10s by default
- Check if collector endpoint is unreachable (events accumulate)
- Manually flush: visit collector, fix issues, reload extension

---

## Example Queries (Collector Side)

Once events are in your data store, example queries:

### Datadog Logs (Log Explorer)

```
source:obs-ux-meter platform:grafana page_type:dashboard
```

```
source:obs-ux-meter action:query_run @query.redaction_applied:true
```

### Grafana Loki (LogQL)

```
{job="obs-ux-meter", platform="grafana"} |= "dashboard"
```

```
{job="obs-ux-meter"} | json | action="route_change"
```

### Generic SQL (if ingested to DB)

```sql
SELECT page_type, COUNT(*) as views
FROM ux_events
WHERE platform = 'grafana'
  AND action = 'route_change'
  AND ts > NOW() - INTERVAL '7 days'
GROUP BY page_type
ORDER BY views DESC;
```

```sql
SELECT anon_user_id, COUNT(DISTINCT session_id) as sessions
FROM ux_events
WHERE ts > NOW() - INTERVAL '30 days'
GROUP BY anon_user_id
HAVING sessions > 5;
```

---

## FAQ

### Is this safe to deploy internally?

**Yes**, with caveats:
- All defaults are privacy-preserving (metadata only)
- Redaction engine strips credentials, secrets, PII
- No network payload interception
- Sampling prevents full surveillance
- **Recommended**: Audit enhanced toggles before enabling

### Can I use this for SaaS analytics?

**No**:
- This is designed for **internal team research** only
- Do not deploy to external users without consent
- Always disclose tracking in privacy policies

### How do I rotate the hash salt?

The hash salt is generated once on installation and stored in `chrome.storage.local`. To rotate:
1. Uninstall the extension
2. Clear extension storage (or manually edit via DevTools → Application → Storage)
3. Reinstall

**Warning**: Rotating salt changes all hashed values (breaks historical correlation).

### Can I disable tracking for myself?

**Yes**:
1. Set sampling rate to 0% (disables all tracking)
2. Or uninstall the extension
3. Or exclude your user ID via backend filtering (if identity is captured)

### Does this work on self-hosted Grafana?

**Yes**, add your host pattern to **Allowed Hosts** in Options:
```
https://grafana.internal.company.com/*
```

### How accurate is page type detection?

**High** for Grafana and Datadog SaaS (based on URL patterns).
**Lower** for heavily customized instances (may require DOM selectors).

See `src/content/grafana.ts` and `src/content/datadog.ts` for detection logic.

---

## License

Internal use only. Not licensed for redistribution.

---

## Contributing

This is an internal tool. For bugs or feature requests, contact the Platform team.

---

## Roadmap

- [ ] Add Prometheus metrics export option
- [ ] Support for custom DOM selectors (per-instance config)
- [ ] Remote config sync (fetch settings from endpoint)
- [ ] Session replay markers (click heatmaps, scroll depth)
- [ ] A/B test variant tracking

---

## Support

For questions or issues:
- **Slack**: #observability-ux
- **Email**: platform-team@example.com
- **Docs**: https://wiki.internal/obs-ux-meter

---

**Built with**: TypeScript, Zod, Vite, Vitest, Playwright, Preact
**Privacy-first** • **Configurable** • **Battle-tested**
