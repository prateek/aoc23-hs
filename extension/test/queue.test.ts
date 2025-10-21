import { describe, it, expect, beforeEach, vi } from 'vitest';
import { EventQueue } from '../src/core/queue';
import type { UXEventV2 } from '../src/core/schema';
import 'fake-indexeddb/auto';

describe('EventQueue', () => {
  let queue: EventQueue;
  let flushHandler: ReturnType<typeof vi.fn>;

  beforeEach(async () => {
    queue = new EventQueue({
      maxBatchSize: 3,
      maxBatchInterval: 1000,
    });
    flushHandler = vi.fn().mockResolvedValue(undefined);

    global.window = {
      setTimeout: vi.fn((fn, ms) => setTimeout(fn as () => void, ms) as unknown as number),
      clearTimeout: vi.fn(clearTimeout),
    } as any;

    await queue.init(flushHandler);
  });

  const createMockEvent = (action: string): UXEventV2 => ({
    ts: new Date().toISOString(),
    session_id: 'test-session',
    anon_user_id: 'test-user',
    platform: 'grafana',
    page_type: 'dashboard',
    action: action as any,
    route: {
      url: 'https://example.com',
      path: '/test',
      query: {},
    },
  });

  it('should enqueue events', async () => {
    await queue.enqueue(createMockEvent('view'));
    expect(queue.getQueueSize()).toBe(1);
  });

  it('should flush when batch size is reached', async () => {
    await queue.enqueue(createMockEvent('view'));
    await queue.enqueue(createMockEvent('view'));
    await queue.enqueue(createMockEvent('view'));

    // Should auto-flush at batch size 3
    await new Promise((resolve) => setTimeout(resolve, 100));

    expect(flushHandler).toHaveBeenCalledWith(
      expect.arrayContaining([
        expect.objectContaining({ action: 'view' }),
        expect.objectContaining({ action: 'view' }),
        expect.objectContaining({ action: 'view' }),
      ])
    );
    expect(queue.getQueueSize()).toBe(0);
  });

  it('should flush after interval', async () => {
    await queue.enqueue(createMockEvent('view'));

    // Wait for flush interval
    await new Promise((resolve) => setTimeout(resolve, 1100));

    expect(flushHandler).toHaveBeenCalledWith(
      expect.arrayContaining([expect.objectContaining({ action: 'view' })])
    );
    expect(queue.getQueueSize()).toBe(0);
  });

  it('should retry on flush failure', async () => {
    flushHandler.mockRejectedValueOnce(new Error('Network error'));

    await queue.enqueue(createMockEvent('view'));
    await queue.enqueue(createMockEvent('view'));
    await queue.enqueue(createMockEvent('view'));

    // Wait for auto-flush
    await new Promise((resolve) => setTimeout(resolve, 100));

    // Events should still be in queue after failure
    expect(queue.getQueueSize()).toBe(3);

    // Second flush should succeed
    flushHandler.mockResolvedValueOnce(undefined);
    await queue.flush();

    expect(queue.getQueueSize()).toBe(0);
  });

  it('should persist to IndexedDB and restore', async () => {
    await queue.enqueue(createMockEvent('view'));
    await queue.enqueue(createMockEvent('route_change'));

    // Create new queue instance (simulating reload)
    const newQueue = new EventQueue({
      maxBatchSize: 3,
      maxBatchInterval: 1000,
    });

    const newFlushHandler = vi.fn().mockResolvedValue(undefined);
    await newQueue.init(newFlushHandler);

    // Should restore events from IndexedDB
    expect(newQueue.getQueueSize()).toBe(2);
  });
});
