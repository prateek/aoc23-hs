import { describe, it, expect, beforeEach, vi } from 'vitest';
import { SPARouter } from '../src/core/router';
import type { Config } from '../src/core/schema';

describe('SPARouter', () => {
  let router: SPARouter;
  let handler: ReturnType<typeof vi.fn>;
  let mockConfig: Config;

  beforeEach(() => {
    mockConfig = {
      redaction_policy: {
        redact_credentials: true,
        redact_emails: true,
        redact_urls: true,
        redact_long_tokens: true,
        allowed_query_params: ['from', 'to'],
      },
    } as Config;

    router = new SPARouter(mockConfig);
    handler = vi.fn();

    // Mock window.history
    global.window = {
      location: {
        href: 'https://example.com/test',
        pathname: '/test',
      },
      history: {
        pushState: vi.fn(),
        replaceState: vi.fn(),
      },
      addEventListener: vi.fn(),
      setTimeout: vi.fn((fn) => {
        fn();
        return 1;
      }),
      clearTimeout: vi.fn(),
    } as any;
  });

  it('should detect initial route', () => {
    router.start(handler);

    expect(handler).toHaveBeenCalledWith(
      expect.objectContaining({
        url: 'https://example.com/test',
        path: '/test',
      })
    );
  });

  it('should filter query params based on allowlist', () => {
    global.window.location = {
      href: 'https://example.com/test?from=now-6h&to=now&secret=abc123',
      pathname: '/test',
    } as any;

    router.start(handler);

    expect(handler).toHaveBeenCalledWith(
      expect.objectContaining({
        query: expect.objectContaining({
          from: 'now-6h',
          to: 'now',
          secret: '', // filtered
        }),
      })
    );
  });

  it('should debounce route changes', () => {
    let timeoutCallback: (() => void) | null = null;
    global.window.setTimeout = vi.fn((fn) => {
      timeoutCallback = fn as () => void;
      return 1;
    }) as any;

    router.start(handler);
    handler.mockClear();

    // Trigger multiple route changes
    router.triggerRouteCheck();
    router.triggerRouteCheck();
    router.triggerRouteCheck();

    expect(handler).not.toHaveBeenCalled();

    // Execute debounced callback
    if (timeoutCallback) timeoutCallback();

    expect(handler).toHaveBeenCalledTimes(1);
  });
});
