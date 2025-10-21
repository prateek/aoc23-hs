import { getConfig, setConfig, uuidv4 } from '../core/utils';
import { ConfigSchema, type Config } from '../core/schema';

// Background service worker for alarms and config management

// Initialize default config on install
chrome.runtime.onInstalled.addListener(async () => {
  console.log('[obs-ux-meter] Extension installed');

  const existingConfig = await getConfig();
  if (!existingConfig) {
    // Set default config
    const defaultConfig: Config = ConfigSchema.parse({
      endpoint_url: undefined,
      auth_token: undefined,
      transport_type: 'generic_http',
      allowed_hosts: [
        'https://*.datadoghq.com/*',
        'https://*.datadoghq.eu/*',
        'https://*.ddog-gov.com/*',
        'https://*.grafana.net/*',
      ],
      sampling_rate: 20,
      privacy_toggles: {
        collect_identity_options: false,
        dom_identity_scrape: false,
        collect_raw_asset_names: false,
        collect_template_var_values: false,
        collect_raw_queries: false,
        collect_absolute_timestamps: false,
        enable_time_bucketing: false,
        enable_performance_tracking: false,
      },
      redaction_policy: {
        redact_credentials: true,
        redact_emails: true,
        redact_urls: true,
        redact_long_tokens: true,
        allowed_query_params: [],
      },
      hash_salt: uuidv4(), // Generate random salt
      debug: false,
      idle_threshold_ms: 60000,
      session_rotation_ms: 86400000, // 24h
    });

    await setConfig(defaultConfig);
    console.log('[obs-ux-meter] Default config initialized');
  }
});

// Setup periodic alarms for maintenance (optional)
chrome.runtime.onStartup.addListener(() => {
  console.log('[obs-ux-meter] Extension started');

  // Setup alarm for periodic config sync or maintenance
  chrome.alarms.create('maintenance', { periodInMinutes: 60 });
});

chrome.alarms.onAlarm.addListener((alarm) => {
  if (alarm.name === 'maintenance') {
    console.log('[obs-ux-meter] Running maintenance');
    // Could fetch remote config here if needed
  }
});

// Handle messages from content scripts or options page
chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
  if (message.type === 'GET_CONFIG') {
    getConfig().then((config) => {
      sendResponse({ config });
    });
    return true; // Keep channel open for async response
  }

  if (message.type === 'SET_CONFIG') {
    setConfig(message.config).then(() => {
      sendResponse({ success: true });
    });
    return true;
  }

  if (message.type === 'SEND_TEST_EVENT') {
    sendResponse({ success: true, message: 'Test event sent (simulated)' });
    return true;
  }
});

export {};
