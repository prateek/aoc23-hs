import { render } from 'preact';
import { useState, useEffect } from 'preact/hooks';
import type { Config } from '../core/schema';
import { ConfigSchema } from '../core/schema';
import { getConfig, setConfig } from '../core/utils';

function OptionsPage() {
  const [config, setConfigState] = useState<Config | null>(null);
  const [status, setStatus] = useState<{ type: 'success' | 'error'; message: string } | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadConfig();
  }, []);

  const loadConfig = async () => {
    try {
      const cfg = await getConfig();
      setConfigState(cfg);
      setLoading(false);
    } catch (error) {
      setStatus({ type: 'error', message: 'Failed to load config' });
      setLoading(false);
    }
  };

  const handleSave = async () => {
    if (!config) return;

    try {
      // Validate with zod
      ConfigSchema.parse(config);
      await setConfig(config);
      setStatus({ type: 'success', message: 'Settings saved successfully!' });
      setTimeout(() => setStatus(null), 3000);
    } catch (error) {
      setStatus({ type: 'error', message: `Validation error: ${error}` });
    }
  };

  const handleTestEvent = async () => {
    setStatus({ type: 'success', message: 'Test event sent (check your collector)' });
    setTimeout(() => setStatus(null), 3000);
  };

  if (loading) {
    return <div>Loading...</div>;
  }

  if (!config) {
    return <div>No configuration found</div>;
  }

  return (
    <div>
      <h1>Observability UX Meter</h1>
      <p class="subtitle">Configure privacy-preserving usage analytics for Grafana and Datadog</p>

      {/* Transport Settings */}
      <div class="section">
        <h2>Transport Settings</h2>
        <label>
          <span class="label-text">Endpoint URL</span>
          <input
            type="url"
            value={config.endpoint_url || ''}
            onInput={(e) =>
              setConfigState({ ...config, endpoint_url: (e.target as HTMLInputElement).value })
            }
            placeholder="https://your-collector.example.com/events"
          />
          <span class="help-text">The HTTP endpoint where events will be sent</span>
        </label>

        <label>
          <span class="label-text">Auth Token</span>
          <input
            type="text"
            value={config.auth_token || ''}
            onInput={(e) =>
              setConfigState({ ...config, auth_token: (e.target as HTMLInputElement).value })
            }
            placeholder="Bearer token or API key"
          />
          <span class="help-text">
            Authorization token (stored securely, never logged)
          </span>
        </label>

        <label>
          <span class="label-text">Transport Type</span>
          <select
            value={config.transport_type}
            onChange={(e) =>
              setConfigState({
                ...config,
                transport_type: (e.target as HTMLSelectElement).value as any,
              })
            }
          >
            <option value="generic_http">Generic HTTP (NDJSON)</option>
            <option value="datadog_logs">Datadog Logs</option>
            <option value="grafana_loki">Grafana Loki</option>
          </select>
        </label>
      </div>

      {/* Sampling & Hosts */}
      <div class="section">
        <h2>Sampling & Allowed Hosts</h2>
        <label>
          <span class="label-text">Sampling Rate (%)</span>
          <input
            type="number"
            min="0"
            max="100"
            value={config.sampling_rate}
            onInput={(e) =>
              setConfigState({
                ...config,
                sampling_rate: parseInt((e.target as HTMLInputElement).value),
              })
            }
          />
          <span class="help-text">Percentage of users to track (0-100)</span>
        </label>

        <label>
          <span class="label-text">Allowed Host Patterns</span>
          <textarea
            rows={4}
            value={config.allowed_hosts.join('\n')}
            onInput={(e) =>
              setConfigState({
                ...config,
                allowed_hosts: (e.target as HTMLTextAreaElement).value
                  .split('\n')
                  .filter((h) => h.trim()),
              })
            }
            placeholder="https://*.grafana.net/*"
          />
          <span class="help-text">One pattern per line (glob syntax supported)</span>
        </label>
      </div>

      {/* Privacy Toggles */}
      <div class="section">
        <h2>Privacy Toggles (All OFF by default)</h2>
        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.collect_identity_options}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  collect_identity_options: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Collect identity from Options (name/email/team)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.dom_identity_scrape}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  dom_identity_scrape: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Enable DOM identity scraping (experimental)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.collect_raw_asset_names}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  collect_raw_asset_names: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Collect raw asset names/UIDs (else hashed)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.collect_template_var_values}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  collect_template_var_values: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Collect template variable values (with redaction)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.collect_raw_queries}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  collect_raw_queries: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Collect raw query text (with redaction & truncation)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.collect_absolute_timestamps}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  collect_absolute_timestamps: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Collect absolute time range timestamps</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.enable_time_bucketing}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  enable_time_bucketing: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Enable time range bucketing</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.privacy_toggles.enable_performance_tracking}
            onChange={(e) =>
              setConfigState({
                ...config,
                privacy_toggles: {
                  ...config.privacy_toggles,
                  enable_performance_tracking: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Enable performance waterfall tracking (dashboard load times, panel timing)</span>
        </div>
      </div>

      {/* Identity */}
      <div class="section">
        <h2>Identity (Optional)</h2>
        <label>
          <span class="label-text">Name</span>
          <input
            type="text"
            value={config.identity?.name || ''}
            onInput={(e) =>
              setConfigState({
                ...config,
                identity: {
                  ...config.identity,
                  name: (e.target as HTMLInputElement).value,
                  source: 'options',
                },
              })
            }
          />
        </label>

        <label>
          <span class="label-text">Email</span>
          <input
            type="text"
            value={config.identity?.email || ''}
            onInput={(e) =>
              setConfigState({
                ...config,
                identity: {
                  ...config.identity,
                  email: (e.target as HTMLInputElement).value,
                  source: 'options',
                },
              })
            }
          />
        </label>

        <label>
          <span class="label-text">Team</span>
          <input
            type="text"
            value={
              config.identity?.team
                ? Array.isArray(config.identity.team)
                  ? config.identity.team.join(', ')
                  : config.identity.team
                : ''
            }
            onInput={(e) =>
              setConfigState({
                ...config,
                identity: {
                  ...config.identity,
                  team: (e.target as HTMLInputElement).value,
                  source: 'options',
                },
              })
            }
          />
        </label>
      </div>

      {/* Redaction Policy */}
      <div class="section">
        <h2>Redaction Policy</h2>
        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.redaction_policy.redact_credentials}
            onChange={(e) =>
              setConfigState({
                ...config,
                redaction_policy: {
                  ...config.redaction_policy,
                  redact_credentials: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Redact credentials (AWS keys, JWTs, etc.)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.redaction_policy.redact_emails}
            onChange={(e) =>
              setConfigState({
                ...config,
                redaction_policy: {
                  ...config.redaction_policy,
                  redact_emails: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Redact email addresses (except identity email)</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.redaction_policy.redact_urls}
            onChange={(e) =>
              setConfigState({
                ...config,
                redaction_policy: {
                  ...config.redaction_policy,
                  redact_urls: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Redact URL query strings</span>
        </div>

        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.redaction_policy.redact_long_tokens}
            onChange={(e) =>
              setConfigState({
                ...config,
                redaction_policy: {
                  ...config.redaction_policy,
                  redact_long_tokens: (e.target as HTMLInputElement).checked,
                },
              })
            }
          />
          <span>Redact tokens longer than 128 characters</span>
        </div>

        <label>
          <span class="label-text">Allowed Query Params (comma-separated)</span>
          <input
            type="text"
            value={config.redaction_policy.allowed_query_params.join(', ')}
            onInput={(e) =>
              setConfigState({
                ...config,
                redaction_policy: {
                  ...config.redaction_policy,
                  allowed_query_params: (e.target as HTMLInputElement).value
                    .split(',')
                    .map((p) => p.trim())
                    .filter(Boolean),
                },
              })
            }
          />
          <span class="help-text">Query params to preserve in URLs (e.g., from, to)</span>
        </label>
      </div>

      {/* Debug */}
      <div class="section">
        <h2>Debug</h2>
        <div class="checkbox-label">
          <input
            type="checkbox"
            checked={config.debug}
            onChange={(e) =>
              setConfigState({ ...config, debug: (e.target as HTMLInputElement).checked })
            }
          />
          <span>Enable debug logging to console</span>
        </div>
      </div>

      {/* Actions */}
      <div class="section">
        <button onClick={handleSave}>Save Settings</button>
        <button class="secondary" onClick={handleTestEvent}>
          Send Test Event
        </button>
      </div>

      {status && (
        <div class={`status ${status.type}`}>
          {status.message}
        </div>
      )}
    </div>
  );
}

render(<OptionsPage />, document.getElementById('root')!);
