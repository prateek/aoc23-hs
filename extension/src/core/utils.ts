import type { Config } from './schema';

// UUID v4 generator
export function uuidv4(): string {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
    const r = (Math.random() * 16) | 0;
    const v = c === 'x' ? r : (r & 0x3) | 0x8;
    return v.toString(16);
  });
}

// SHA-256 hash with salt
export async function hashWithSalt(value: string, salt: string): Promise<string> {
  const encoder = new TextEncoder();
  const data = encoder.encode(value + salt);
  const hashBuffer = await crypto.subtle.digest('SHA-256', data);
  const hashArray = Array.from(new Uint8Array(hashBuffer));
  return hashArray.map((b) => b.toString(16).padStart(2, '0')).join('');
}

// Hash asset identifiers if required
export async function maybeHashAsset(
  value: string,
  config: Config,
  shouldHash: boolean
): Promise<string> {
  if (!shouldHash || config.privacy_toggles.collect_raw_asset_names) {
    return value;
  }
  return hashWithSalt(value, config.hash_salt);
}

// Parse URL safely
export function parseURL(url: string): {
  path: string;
  query: Record<string, string | string[]>;
} {
  try {
    const parsed = new URL(url);
    const query: Record<string, string | string[]> = {};

    parsed.searchParams.forEach((value, key) => {
      if (query[key]) {
        if (Array.isArray(query[key])) {
          (query[key] as string[]).push(value);
        } else {
          query[key] = [query[key] as string, value];
        }
      } else {
        query[key] = value;
      }
    });

    return {
      path: parsed.pathname,
      query,
    };
  } catch {
    return {
      path: url,
      query: {},
    };
  }
}

// Filter query params based on allowlist
export function filterQueryParams(
  query: Record<string, string | string[]>,
  allowlist: string[]
): Record<string, string | string[]> {
  if (allowlist.length === 0) {
    // Return keys only (values as empty string)
    return Object.fromEntries(Object.keys(query).map((k) => [k, '']));
  }

  const filtered: Record<string, string | string[]> = {};
  for (const key of Object.keys(query)) {
    if (allowlist.includes(key)) {
      filtered[key] = query[key];
    } else {
      filtered[key] = '';
    }
  }
  return filtered;
}

// Storage helpers
const STORAGE_KEYS = {
  CONFIG: 'obs_ux_config',
  ANON_USER_ID: 'obs_ux_anon_user_id',
  SESSION_ID: 'obs_ux_session_id',
  SESSION_START: 'obs_ux_session_start',
  LAST_ACTIVITY: 'obs_ux_last_activity',
} as const;

export async function getConfig(): Promise<Config | null> {
  const result = await chrome.storage.local.get(STORAGE_KEYS.CONFIG);
  return result[STORAGE_KEYS.CONFIG] || null;
}

export async function setConfig(config: Config): Promise<void> {
  await chrome.storage.local.set({ [STORAGE_KEYS.CONFIG]: config });
}

export async function getOrCreateAnonUserId(): Promise<string> {
  const result = await chrome.storage.local.get(STORAGE_KEYS.ANON_USER_ID);
  let anonUserId = result[STORAGE_KEYS.ANON_USER_ID];
  if (!anonUserId) {
    anonUserId = uuidv4();
    await chrome.storage.local.set({ [STORAGE_KEYS.ANON_USER_ID]: anonUserId });
  }
  return anonUserId;
}

export async function getOrCreateSessionId(config: Config): Promise<string> {
  const now = Date.now();
  const result = await chrome.storage.local.get([
    STORAGE_KEYS.SESSION_ID,
    STORAGE_KEYS.SESSION_START,
    STORAGE_KEYS.LAST_ACTIVITY,
  ]);

  const sessionId = result[STORAGE_KEYS.SESSION_ID];
  const sessionStart = result[STORAGE_KEYS.SESSION_START];
  const lastActivity = result[STORAGE_KEYS.LAST_ACTIVITY];

  // Rotate if session expired (24h) or idle timeout (30min)
  const sessionExpired = sessionStart && now - sessionStart > config.session_rotation_ms;
  const idleExpired = lastActivity && now - lastActivity > config.idle_threshold_ms;

  if (!sessionId || sessionExpired || idleExpired) {
    const newSessionId = uuidv4();
    await chrome.storage.local.set({
      [STORAGE_KEYS.SESSION_ID]: newSessionId,
      [STORAGE_KEYS.SESSION_START]: now,
      [STORAGE_KEYS.LAST_ACTIVITY]: now,
    });
    return newSessionId;
  }

  // Update last activity
  await chrome.storage.local.set({ [STORAGE_KEYS.LAST_ACTIVITY]: now });
  return sessionId;
}

export async function updateLastActivity(): Promise<void> {
  await chrome.storage.local.set({ [STORAGE_KEYS.LAST_ACTIVITY]: Date.now() });
}

// Sampling decision based on anon_user_id hash
export function shouldSample(anonUserId: string, samplingRate: number): boolean {
  if (samplingRate >= 100) return true;
  if (samplingRate <= 0) return false;

  // Deterministic sampling based on UUID
  const hash = anonUserId.split('-').join('');
  const numericHash = parseInt(hash.substring(0, 8), 16);
  const bucket = numericHash % 100;
  return bucket < samplingRate;
}

// Debug logger
export function debug(config: Config | null, ...args: unknown[]): void {
  if (config?.debug) {
    console.debug('[obs-ux-meter]', ...args);
  }
}

// Match host against patterns (simple glob)
export function matchHost(url: string, patterns: string[]): boolean {
  try {
    const parsed = new URL(url);
    const host = parsed.host;
    const fullUrl = url;

    return patterns.some((pattern) => {
      // Simple glob matching: convert * to .*
      const regex = new RegExp('^' + pattern.replace(/\*/g, '.*').replace(/\?/g, '.') + '$');
      return regex.test(fullUrl) || regex.test(host);
    });
  } catch {
    return false;
  }
}

// Truncate string safely
export function truncate(str: string, maxLength: number): string {
  if (str.length <= maxLength) return str;
  return str.substring(0, maxLength);
}
