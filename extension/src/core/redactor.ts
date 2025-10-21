import type { RedactionPolicy } from './schema';

export interface RedactionResult {
  text: string;
  applied: boolean;
}

// Redaction patterns
const PATTERNS = {
  // AWS credentials: AKIA... or ASIA...
  AWS_ACCESS_KEY: /(?:AKIA|ASIA)[0-9A-Z]{16}/gi,

  // Google API keys
  GOOGLE_API_KEY: /AIza[0-9A-Za-z_-]{35}/gi,

  // JWT tokens (xxxxx.yyyyy.zzzzz)
  JWT: /eyJ[A-Za-z0-9_-]+\.eyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+/gi,

  // Long hex strings (likely tokens)
  LONG_HEX: /\b[0-9a-fA-F]{32,}\b/g,

  // Bearer tokens
  BEARER: /Bearer\s+\S+/gi,

  // Password/token/secret assignments
  SECRET_ASSIGN: /(password|token|secret|api[_-]?key)\s*[:=]\s*['"]?[^\s'"]+/gi,

  // Email addresses (but we'll handle identity.email separately)
  EMAIL: /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/g,

  // Private keys
  PRIVATE_KEY: /-----BEGIN\s+(?:RSA\s+)?PRIVATE\s+KEY-----[\s\S]*?-----END\s+(?:RSA\s+)?PRIVATE\s+KEY-----/gi,
};

export function redact(text: string, policy: RedactionPolicy, allowedEmail?: string): RedactionResult {
  let result = text;
  let applied = false;

  if (policy.redact_credentials) {
    // AWS keys
    if (PATTERNS.AWS_ACCESS_KEY.test(result)) {
      result = result.replace(PATTERNS.AWS_ACCESS_KEY, '<AWS_KEY>');
      applied = true;
    }

    // Google keys
    if (PATTERNS.GOOGLE_API_KEY.test(result)) {
      result = result.replace(PATTERNS.GOOGLE_API_KEY, '<GOOGLE_KEY>');
      applied = true;
    }

    // JWT tokens
    if (PATTERNS.JWT.test(result)) {
      result = result.replace(PATTERNS.JWT, '<JWT_TOKEN>');
      applied = true;
    }

    // Long hex
    if (PATTERNS.LONG_HEX.test(result)) {
      result = result.replace(PATTERNS.LONG_HEX, '<HEX_TOKEN>');
      applied = true;
    }

    // Bearer tokens
    if (PATTERNS.BEARER.test(result)) {
      result = result.replace(PATTERNS.BEARER, 'Bearer <REDACTED>');
      applied = true;
    }

    // Secret assignments
    if (PATTERNS.SECRET_ASSIGN.test(result)) {
      result = result.replace(PATTERNS.SECRET_ASSIGN, (match) => {
        const [key] = match.split(/[:=]/);
        return `${key.trim()}=<REDACTED>`;
      });
      applied = true;
    }

    // Private keys
    if (PATTERNS.PRIVATE_KEY.test(result)) {
      result = result.replace(PATTERNS.PRIVATE_KEY, '<PRIVATE_KEY>');
      applied = true;
    }
  }

  if (policy.redact_emails) {
    // Redact all emails except the allowed one (identity.email)
    const emails = result.match(PATTERNS.EMAIL);
    if (emails) {
      emails.forEach((email) => {
        if (email !== allowedEmail) {
          result = result.replace(email, '***@***');
          applied = true;
        }
      });
    }
  }

  if (policy.redact_long_tokens) {
    // Redact any single token >128 chars
    const tokens = result.split(/\s+/);
    tokens.forEach((token) => {
      if (token.length > 128) {
        result = result.replace(token, '<LONG_TOKEN>');
        applied = true;
      }
    });
  }

  if (policy.redact_urls) {
    // Strip query strings from URLs (except allowed params)
    // This is a simple implementation; more sophisticated URL parsing could be added
    const urlPattern = /https?:\/\/[^\s]+/g;
    const urls = result.match(urlPattern);
    if (urls) {
      urls.forEach((url) => {
        try {
          const parsed = new URL(url);
          const allowedParams = policy.allowed_query_params || [];

          if (parsed.search && allowedParams.length === 0) {
            // Strip all query params
            const cleanUrl = `${parsed.origin}${parsed.pathname}`;
            result = result.replace(url, cleanUrl);
            applied = true;
          } else if (parsed.search) {
            // Keep only allowed params
            const newParams = new URLSearchParams();
            parsed.searchParams.forEach((value, key) => {
              if (allowedParams.includes(key)) {
                newParams.set(key, value);
              }
            });
            const cleanUrl = `${parsed.origin}${parsed.pathname}${
              newParams.toString() ? '?' + newParams.toString() : ''
            }`;
            if (cleanUrl !== url) {
              result = result.replace(url, cleanUrl);
              applied = true;
            }
          }
        } catch {
          // Invalid URL, skip
        }
      });
    }
  }

  return { text: result, applied };
}

// Redact and truncate for template variables
export function redactAndTruncateVar(
  value: string,
  policy: RedactionPolicy,
  maxLength = 256
): RedactionResult {
  const redacted = redact(value, policy);
  if (redacted.text.length > maxLength) {
    return {
      text: redacted.text.substring(0, maxLength),
      applied: true,
    };
  }
  return redacted;
}

// Redact and truncate for queries
export function redactAndTruncateQuery(
  value: string,
  policy: RedactionPolicy,
  maxLength = 500
): RedactionResult {
  const redacted = redact(value, policy);
  if (redacted.text.length > maxLength) {
    return {
      text: redacted.text.substring(0, maxLength),
      applied: true,
    };
  }
  return redacted;
}
