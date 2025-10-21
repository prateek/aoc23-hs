export type IdleStateChangeHandler = (isIdle: boolean) => void;
export type HeartbeatHandler = () => void;

export class IdleTracker {
  private isIdle = false;
  private idleThreshold: number;
  private heartbeatInterval: number;
  private lastActivity = Date.now();
  private idleTimer: number | null = null;
  private heartbeatTimer: number | null = null;
  private stateChangeHandler: IdleStateChangeHandler | null = null;
  private heartbeatHandler: HeartbeatHandler | null = null;

  constructor(idleThreshold = 60000, heartbeatInterval = 15000) {
    this.idleThreshold = idleThreshold;
    this.heartbeatInterval = heartbeatInterval;
  }

  start(
    stateChangeHandler: IdleStateChangeHandler,
    heartbeatHandler: HeartbeatHandler
  ): void {
    this.stateChangeHandler = stateChangeHandler;
    this.heartbeatHandler = heartbeatHandler;

    // Listen to user activity
    this.attachActivityListeners();

    // Start idle detection
    this.resetIdleTimer();

    // Start heartbeat
    this.startHeartbeat();

    // Listen to visibility changes
    document.addEventListener('visibilitychange', this.handleVisibilityChange);
  }

  stop(): void {
    this.detachActivityListeners();
    this.stopIdleTimer();
    this.stopHeartbeat();
    document.removeEventListener('visibilitychange', this.handleVisibilityChange);
  }

  getIsIdle(): boolean {
    return this.isIdle;
  }

  getLastActivity(): number {
    return this.lastActivity;
  }

  private attachActivityListeners(): void {
    const events = ['mousedown', 'keydown', 'scroll', 'touchstart', 'click'];
    events.forEach((event) => {
      document.addEventListener(event, this.handleActivity, { passive: true });
    });
  }

  private detachActivityListeners(): void {
    const events = ['mousedown', 'keydown', 'scroll', 'touchstart', 'click'];
    events.forEach((event) => {
      document.removeEventListener(event, this.handleActivity);
    });
  }

  private handleActivity = (): void => {
    this.lastActivity = Date.now();

    if (this.isIdle) {
      this.isIdle = false;
      this.stateChangeHandler?.(false);
    }

    this.resetIdleTimer();
  };

  private resetIdleTimer(): void {
    this.stopIdleTimer();

    this.idleTimer = window.setTimeout(() => {
      if (!this.isIdle) {
        this.isIdle = true;
        this.stateChangeHandler?.(true);
      }
    }, this.idleThreshold);
  }

  private stopIdleTimer(): void {
    if (this.idleTimer) {
      clearTimeout(this.idleTimer);
      this.idleTimer = null;
    }
  }

  private startHeartbeat(): void {
    this.heartbeatTimer = window.setInterval(() => {
      if (!this.isIdle && document.visibilityState === 'visible') {
        this.heartbeatHandler?.();
      }
    }, this.heartbeatInterval);
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }

  private handleVisibilityChange = (): void => {
    if (document.visibilityState === 'hidden') {
      // Trigger flush on visibility hidden
      this.heartbeatHandler?.();
    } else {
      // Reset activity on visibility visible
      this.handleActivity();
    }
  };
}
