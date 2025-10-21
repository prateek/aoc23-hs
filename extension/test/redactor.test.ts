import { describe, it, expect } from 'vitest';
import { redact, redactAndTruncateVar, redactAndTruncateQuery } from '../src/core/redactor';
import type { RedactionPolicy } from '../src/core/schema';

describe('Redactor', () => {
  const defaultPolicy: RedactionPolicy = {
    redact_credentials: true,
    redact_emails: true,
    redact_urls: true,
    redact_long_tokens: true,
    allowed_query_params: [],
  };

  describe('AWS keys', () => {
    it('should redact AWS access keys', () => {
      const text = 'My key is AKIAIOSFODNN7EXAMPLE';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('My key is <AWS_KEY>');
      expect(result.applied).toBe(true);
    });

    it('should redact AWS session keys', () => {
      const text = 'Session: ASIAIOSFODNN7EXAMPLE';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('Session: <AWS_KEY>');
      expect(result.applied).toBe(true);
    });
  });

  describe('JWT tokens', () => {
    it('should redact JWT tokens', () => {
      const text = 'Token: eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U';
      const result = redact(text, defaultPolicy);

      expect(result.text).toContain('<JWT_TOKEN>');
      expect(result.applied).toBe(true);
    });
  });

  describe('Long hex tokens', () => {
    it('should redact long hex strings', () => {
      const text = 'Hash: 0123456789abcdef0123456789abcdef';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('Hash: <HEX_TOKEN>');
      expect(result.applied).toBe(true);
    });

    it('should not redact short hex strings', () => {
      const text = 'Hash: abc123';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('Hash: abc123');
      expect(result.applied).toBe(false);
    });
  });

  describe('Bearer tokens', () => {
    it('should redact Bearer tokens', () => {
      const text = 'Authorization: Bearer sk_test_123456789';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('Authorization: Bearer <REDACTED>');
      expect(result.applied).toBe(true);
    });
  });

  describe('Secret assignments', () => {
    it('should redact password assignments', () => {
      const text = 'password=mysecretpass';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('password=<REDACTED>');
      expect(result.applied).toBe(true);
    });

    it('should redact token assignments', () => {
      const text = 'api_key: "sk_live_123"';
      const result = redact(text, defaultPolicy);

      expect(result.text).toContain('api_key=<REDACTED>');
      expect(result.applied).toBe(true);
    });
  });

  describe('Email addresses', () => {
    it('should redact emails', () => {
      const text = 'Contact: user@example.com';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('Contact: ***@***');
      expect(result.applied).toBe(true);
    });

    it('should preserve allowed email', () => {
      const text = 'User: allowed@example.com, Other: other@example.com';
      const result = redact(text, defaultPolicy, 'allowed@example.com');

      expect(result.text).toContain('allowed@example.com');
      expect(result.text).toContain('***@***');
      expect(result.applied).toBe(true);
    });
  });

  describe('Long tokens', () => {
    it('should redact tokens longer than 128 chars', () => {
      const longToken = 'a'.repeat(150);
      const text = `Token: ${longToken}`;
      const result = redact(text, defaultPolicy);

      expect(result.text).toContain('<LONG_TOKEN>');
      expect(result.applied).toBe(true);
    });
  });

  describe('URLs', () => {
    it('should strip query strings from URLs', () => {
      const text = 'Visit https://example.com/path?secret=abc123&id=456';
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('Visit https://example.com/path');
      expect(result.applied).toBe(true);
    });

    it('should preserve allowed query params', () => {
      const policy = { ...defaultPolicy, allowed_query_params: ['id'] };
      const text = 'Visit https://example.com/path?secret=abc123&id=456';
      const result = redact(text, policy);

      expect(result.text).toContain('id=456');
      expect(result.text).not.toContain('secret');
      expect(result.applied).toBe(true);
    });
  });

  describe('Private keys', () => {
    it('should redact RSA private keys', () => {
      const text = `-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEA...
-----END RSA PRIVATE KEY-----`;
      const result = redact(text, defaultPolicy);

      expect(result.text).toBe('<PRIVATE_KEY>');
      expect(result.applied).toBe(true);
    });
  });

  describe('Truncation', () => {
    it('should truncate variables to max length', () => {
      const longValue = 'a'.repeat(300);
      const result = redactAndTruncateVar(longValue, defaultPolicy, 256);

      expect(result.text.length).toBe(256);
      expect(result.applied).toBe(true);
    });

    it('should truncate queries to max length', () => {
      const longQuery = 'SELECT * FROM logs WHERE ' + 'condition AND '.repeat(100);
      const result = redactAndTruncateQuery(longQuery, defaultPolicy, 500);

      expect(result.text.length).toBe(500);
      expect(result.applied).toBe(true);
    });
  });

  describe('Multiple patterns', () => {
    it('should apply multiple redactions', () => {
      const text = 'AWS: AKIAIOSFODNN7EXAMPLE, Email: user@example.com, Token: Bearer sk_123';
      const result = redact(text, defaultPolicy);

      expect(result.text).toContain('<AWS_KEY>');
      expect(result.text).toContain('***@***');
      expect(result.text).toContain('Bearer <REDACTED>');
      expect(result.applied).toBe(true);
    });
  });

  describe('Disabled policies', () => {
    it('should not redact when policies are disabled', () => {
      const policy: RedactionPolicy = {
        redact_credentials: false,
        redact_emails: false,
        redact_urls: false,
        redact_long_tokens: false,
        allowed_query_params: [],
      };

      const text = 'AWS: AKIAIOSFODNN7EXAMPLE, Email: user@example.com';
      const result = redact(text, policy);

      expect(result.text).toBe(text);
      expect(result.applied).toBe(false);
    });
  });
});
