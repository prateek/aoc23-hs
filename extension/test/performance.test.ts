import { describe, it, expect, beforeEach, vi } from 'vitest';
import { PerformanceTracker } from '../src/core/performance';
import type { Config } from '../src/core/schema';

describe('PerformanceTracker', () => {
  let tracker: PerformanceTracker;
  let mockConfig: Config;

  beforeEach(() => {
    mockConfig = {
      privacy_toggles: {
        enable_performance_tracking: true,
      },
    } as Config;

    tracker = new PerformanceTracker(mockConfig);

    // Mock performance API
    global.performance = {
      now: vi.fn(() => Date.now()),
      getEntriesByType: vi.fn(() => []),
    } as any;

    global.PerformanceObserver = vi.fn() as any;
  });

  it('should start and end a span', () => {
    tracker.startSpan('test-span', { type: 'panel', assetId: 'panel-1' });
    tracker.endSpan('test-span');

    const waterfall = tracker.getWaterfall();
    expect(waterfall).toBeDefined();
    expect(waterfall?.spans).toHaveLength(1);
    expect(waterfall?.spans[0].name).toBe('test-span');
    expect(waterfall?.spans[0].type).toBe('panel');
  });

  it('should measure synchronous functions', () => {
    const result = tracker.measure(
      'sync-test',
      { type: 'render' },
      () => {
        return 42;
      }
    );

    expect(result).toBe(42);

    const waterfall = tracker.getWaterfall();
    expect(waterfall?.spans).toHaveLength(1);
    expect(waterfall?.spans[0].name).toBe('sync-test');
  });

  it('should measure async functions', async () => {
    const result = await tracker.measureAsync(
      'async-test',
      { type: 'data-fetch' },
      async () => {
        return Promise.resolve('done');
      }
    );

    expect(result).toBe('done');

    const waterfall = tracker.getWaterfall();
    expect(waterfall?.spans).toHaveLength(1);
    expect(waterfall?.spans[0].name).toBe('async-test');
  });

  it('should mark spans with custom timing', () => {
    tracker.markSpan('custom-span', { type: 'query' }, 100, 250);

    const waterfall = tracker.getWaterfall();
    expect(waterfall?.spans).toHaveLength(1);
    expect(waterfall?.spans[0].start_ms).toBe(100);
    expect(waterfall?.spans[0].duration_ms).toBe(250);
  });

  it('should calculate total duration correctly', () => {
    tracker.markSpan('span-1', { type: 'panel' }, 0, 100);
    tracker.markSpan('span-2', { type: 'panel' }, 50, 200);

    const waterfall = tracker.getWaterfall();
    expect(waterfall?.total_duration_ms).toBe(250); // max(0+100, 50+200) = 250
  });

  it('should reset and clear spans', () => {
    tracker.markSpan('span-1', { type: 'panel' }, 0, 100);
    tracker.reset();

    const waterfall = tracker.getWaterfall();
    expect(waterfall).toBeNull();
  });

  it('should return null waterfall when disabled', () => {
    const disabledConfig = {
      privacy_toggles: {
        enable_performance_tracking: false,
      },
    } as Config;

    const disabledTracker = new PerformanceTracker(disabledConfig);
    disabledTracker.startSpan('test', { type: 'panel' });
    disabledTracker.endSpan('test');

    const waterfall = disabledTracker.getWaterfall();
    expect(waterfall).toBeNull();
  });

  it('should include metadata in spans', () => {
    tracker.markSpan(
      'panel-with-meta',
      { type: 'panel', assetId: 'panel-123', metadata: { name: 'CPU Usage', rows: 10 } },
      0,
      150
    );

    const waterfall = tracker.getWaterfall();
    expect(waterfall?.spans[0].metadata).toEqual({ name: 'CPU Usage', rows: 10 });
  });

  it('should sort spans by start time', () => {
    tracker.markSpan('span-2', { type: 'panel' }, 100, 50);
    tracker.markSpan('span-1', { type: 'panel' }, 50, 50);
    tracker.markSpan('span-3', { type: 'panel' }, 150, 50);

    const waterfall = tracker.getWaterfall();
    expect(waterfall?.spans.map((s) => s.name)).toEqual(['span-1', 'span-2', 'span-3']);
  });
});
