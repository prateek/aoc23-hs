import { openDB, type IDBPDatabase } from 'idb';
import type { UXEventV2 } from './schema';

const DB_NAME = 'obs_ux_meter_db';
const STORE_NAME = 'events_queue';
const DB_VERSION = 1;

export interface QueueOptions {
  maxBatchSize: number;
  maxBatchInterval: number; // ms
}

export type FlushHandler = (events: UXEventV2[]) => Promise<void>;

export class EventQueue {
  private queue: UXEventV2[] = [];
  private db: IDBPDatabase | null = null;
  private flushHandler: FlushHandler | null = null;
  private options: QueueOptions;
  private flushTimer: number | null = null;
  private isFlushing = false;

  constructor(options: QueueOptions) {
    this.options = options;
  }

  async init(flushHandler: FlushHandler): Promise<void> {
    this.flushHandler = flushHandler;
    this.db = await openDB(DB_NAME, DB_VERSION, {
      upgrade(db) {
        if (!db.objectStoreNames.contains(STORE_NAME)) {
          db.createObjectStore(STORE_NAME, { keyPath: 'id', autoIncrement: true });
        }
      },
    });

    // Load persisted events on init
    await this.loadFromDB();
    this.scheduleFlush();
  }

  async enqueue(event: UXEventV2): Promise<void> {
    this.queue.push(event);

    // Persist to IndexedDB
    if (this.db) {
      await this.db.add(STORE_NAME, { ...event, id: undefined });
    }

    // Check if we should flush immediately
    if (this.queue.length >= this.options.maxBatchSize) {
      await this.flush();
    } else {
      this.scheduleFlush();
    }
  }

  async flush(): Promise<void> {
    if (this.isFlushing || this.queue.length === 0 || !this.flushHandler) {
      return;
    }

    this.isFlushing = true;
    this.cancelScheduledFlush();

    const batch = [...this.queue];
    this.queue = [];

    try {
      await this.flushHandler(batch);
      // Clear from IndexedDB on success
      await this.clearDB();
    } catch (error) {
      console.error('[obs-ux-meter] Flush failed, events will retry:', error);
      // Re-queue events on failure
      this.queue.unshift(...batch);
    } finally {
      this.isFlushing = false;
      if (this.queue.length > 0) {
        this.scheduleFlush();
      }
    }
  }

  // Force flush with sendBeacon (for unload)
  async flushWithBeacon(url: string, authToken?: string): Promise<void> {
    if (this.queue.length === 0) return;

    const batch = [...this.queue];
    this.queue = [];

    const ndjson = batch.map((e) => JSON.stringify(e)).join('\n');
    const blob = new Blob([ndjson], { type: 'application/x-ndjson' });

    // sendBeacon doesn't support custom headers, so we append token to URL if needed
    const beaconUrl = authToken ? `${url}?token=${encodeURIComponent(authToken)}` : url;

    const sent = navigator.sendBeacon(beaconUrl, blob);
    if (!sent) {
      console.error('[obs-ux-meter] sendBeacon failed');
    }

    // Clear DB regardless (best effort)
    await this.clearDB();
  }

  private scheduleFlush(): void {
    if (this.flushTimer) return;

    this.flushTimer = window.setTimeout(() => {
      this.flush();
    }, this.options.maxBatchInterval);
  }

  private cancelScheduledFlush(): void {
    if (this.flushTimer) {
      clearTimeout(this.flushTimer);
      this.flushTimer = null;
    }
  }

  private async loadFromDB(): Promise<void> {
    if (!this.db) return;

    const tx = this.db.transaction(STORE_NAME, 'readonly');
    const store = tx.objectStore(STORE_NAME);
    const allEvents = await store.getAll();

    this.queue = allEvents.map((e) => {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      const { id, ...event } = e;
      return event as UXEventV2;
    });
  }

  private async clearDB(): Promise<void> {
    if (!this.db) return;

    const tx = this.db.transaction(STORE_NAME, 'readwrite');
    await tx.objectStore(STORE_NAME).clear();
    await tx.done;
  }

  getQueueSize(): number {
    return this.queue.length;
  }
}
