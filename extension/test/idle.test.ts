import { describe, it, expect, beforeEach, vi } from 'vitest';
import { IdleTracker } from '../src/core/idle';

describe('IdleTracker', () => {
  let tracker: IdleTracker;
  let stateHandler: ReturnType<typeof vi.fn>;
  let heartbeatHandler: ReturnType<typeof vi.fn>;

  beforeEach(() => {
    tracker = new IdleTracker(1000, 500); // 1s idle, 500ms heartbeat
    stateHandler = vi.fn();
    heartbeatHandler = vi.fn();

    // Mock DOM
    global.document = {
      addEventListener: vi.fn(),
      removeEventListener: vi.fn(),
      visibilityState: 'visible',
    } as any;

    global.window = {
      setTimeout: vi.fn((fn, ms) => {
        return setTimeout(fn as () => void, ms) as unknown as number;
      }),
      setInterval: vi.fn((fn, ms) => {
        return setInterval(fn as () => void, ms) as unknown as number;
      }),
      clearTimeout: vi.fn(clearTimeout),
      clearInterval: vi.fn(clearInterval),
    } as any;
  });

  it('should start in non-idle state', () => {
    tracker.start(stateHandler, heartbeatHandler);
    expect(tracker.getIsIdle()).toBe(false);
  });

  it('should transition to idle after threshold', async () => {
    tracker.start(stateHandler, heartbeatHandler);

    // Wait for idle threshold
    await new Promise((resolve) => setTimeout(resolve, 1100));

    expect(stateHandler).toHaveBeenCalledWith(true);
    expect(tracker.getIsIdle()).toBe(true);
  });

  it('should reset idle on activity', async () => {
    tracker.start(stateHandler, heartbeatHandler);

    // Transition to idle
    await new Promise((resolve) => setTimeout(resolve, 1100));
    expect(tracker.getIsIdle()).toBe(true);

    // Simulate activity
    const activityHandler = (document.addEventListener as any).mock.calls.find(
      (call: any[]) => call[0] === 'mousedown'
    )?.[1];

    if (activityHandler) {
      activityHandler();
    }

    expect(stateHandler).toHaveBeenCalledWith(false);
    expect(tracker.getIsIdle()).toBe(false);
  });

  it('should call heartbeat at intervals when active', async () => {
    tracker.start(stateHandler, heartbeatHandler);

    // Wait for multiple heartbeats
    await new Promise((resolve) => setTimeout(resolve, 600));

    expect(heartbeatHandler).toHaveBeenCalled();
  });

  it('should not call heartbeat when idle', async () => {
    tracker.start(stateHandler, heartbeatHandler);

    // Transition to idle
    await new Promise((resolve) => setTimeout(resolve, 1100));
    heartbeatHandler.mockClear();

    // Wait for heartbeat interval
    await new Promise((resolve) => setTimeout(resolve, 600));

    expect(heartbeatHandler).not.toHaveBeenCalled();
  });
});
