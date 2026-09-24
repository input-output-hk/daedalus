import { randomBytes } from 'crypto';
import { setTimeout, clearTimeout } from 'timers';
import {
  ARIADNE_CONSENT_VERSION,
  ConsentView,
  record,
  validMessage,
} from '../../common/analytics/contract';
import { AnalyticsConfig } from './config';
import { Device, normalizeEvent } from './normalize';
import { Transport } from './transport';

export interface ConsentStorage {
  read(): unknown;
  write(value: unknown): void;
}
type SavedConsent = {
  version: number;
  recipient: string;
  status: 'ACCEPTED' | 'REJECTED' | 'PENDING';
  id?: string;
};
type Queued = {
  body: string;
  bytes: number;
  expires: number;
  generation: number;
};
const uuid = () => {
  const bytes = randomBytes(16);
  bytes[6] = (bytes[6] & 15) | 64;
  bytes[8] = (bytes[8] & 63) | 128;
  const hex = bytes.toString('hex');
  return `${hex.slice(0, 8)}-${hex.slice(8, 12)}-${hex.slice(12, 16)}-${hex.slice(16, 20)}-${hex.slice(20)}`;
};

export class AnalyticsOwner {
  private saved: SavedConsent;
  private generation = 1;
  private fault = false;
  private closed = false;
  private queue: Queued[] = [];
  private bytes = 0;
  private active: AbortController | null = null;
  private timer: ReturnType<typeof setTimeout> | null = null;
  private nextSend = 0;
  private sentAt: number[] = [];
  private failures = 0;
  private attempts = new Map<
    number,
    { id: string; flow: string; started: number; terminal: boolean }
  >();
  private lastAttempt = 0;
  constructor(
    private config: AnalyticsConfig,
    private storage: ConsentStorage,
    private device: Device,
    private transport: Transport,
    private now = Date.now
  ) {
    this.saved = {
      version: ARIADNE_CONSENT_VERSION,
      recipient: config?.endpoint || '',
      status: 'PENDING',
    };
    try {
      const value = storage.read();
      if (
        record(value) &&
        value.version === ARIADNE_CONSENT_VERSION &&
        value.recipient === config?.endpoint &&
        typeof value.status === 'string' &&
        ['PENDING', 'REJECTED', 'ACCEPTED'].includes(value.status)
      ) {
        if (
          value.status !== 'ACCEPTED' ||
          (typeof value.id === 'string' &&
            /^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$/.test(
              value.id
            ))
        )
          this.saved = value as SavedConsent;
      }
    } catch {
      this.fault = true;
    }
  }
  view(): ConsentView {
    return {
      version: ARIADNE_CONSENT_VERSION,
      enabled: !!this.config && !this.closed && !this.fault,
      status: this.config && !this.fault ? this.saved.status : 'REJECTED',
      generation: this.generation,
    };
  }
  consent(command: unknown): ConsentView {
    if (!record(command)) return this.view();
    if (Object.keys(command).length === 1 && command.get === true)
      return this.view();
    if (
      Object.keys(command).length !== 2 ||
      command.version !== ARIADNE_CONSENT_VERSION ||
      typeof command.status !== 'string' ||
      !['PENDING', 'REJECTED', 'ACCEPTED'].includes(command.status)
    )
      return this.view();
    if (command.status === 'ACCEPTED' && (!this.config || this.closed))
      return this.view();
    // Revocation is synchronous in the sender before persistence or any await.
    this.cancel();
    const next: SavedConsent = {
      version: ARIADNE_CONSENT_VERSION,
      recipient: this.config?.endpoint || '',
      status: command.status as SavedConsent['status'],
    };
    if (next.status === 'ACCEPTED')
      next.id =
        this.saved.status === 'ACCEPTED' && this.saved.id
          ? this.saved.id
          : uuid();
    this.saved = next;
    try {
      this.storage.write(next);
      this.fault = false;
    } catch {
      this.fault = true;
      throw new Error('Analytics choice could not be saved');
    }
    return this.view();
  }
  enqueue(input: unknown): boolean {
    if (
      !this.view().enabled ||
      this.saved.status !== 'ACCEPTED' ||
      !record(input) ||
      input.generation !== this.generation
    )
      return false;
    if (!validMessage(input, this.now())) return false;
    // Bound transient attempt ownership; no renderer UUID is accepted.
    for (const [key, value] of this.attempts)
      if (value.started + 30 * 60_000 < this.now()) this.attempts.delete(key);
    let attempt:
      | { id: string; flow: string; started: number; terminal: boolean }
      | undefined;
    if (input.type === 'funnel_step') {
      attempt = this.attempts.get(input.attempt);
      if (input.stage === 'started') {
        if (
          attempt ||
          input.attempt <= this.lastAttempt ||
          this.attempts.size >= 8
        )
          return false;
        attempt = {
          id: uuid(),
          flow: input.action,
          started: Date.parse(input.ts),
          terminal: false,
        };
      } else if (
        !attempt ||
        attempt.terminal ||
        attempt.flow !== input.action ||
        Date.parse(input.ts) < attempt.started
      )
        return false;
    }
    const event = normalizeEvent(
      input,
      this.saved.id,
      this.device,
      this.now(),
      attempt?.id
    );
    if (!event) return false;
    const body = JSON.stringify(event);
    const bytes = Buffer.byteLength(body);
    if (bytes > 2048 || this.queue.length >= 32 || this.bytes + bytes > 65536)
      return false;
    if (input.type === 'funnel_step') {
      if (input.stage === 'started') {
        this.lastAttempt = input.attempt;
        this.attempts.set(input.attempt, attempt);
      } else this.attempts.delete(input.attempt);
    }
    this.queue.push({
      body,
      bytes,
      expires: Date.parse(event.ts) + 30_000,
      generation: this.generation,
    });
    this.bytes += bytes;
    this.pump();
    return true; // Memory admission only; never a delivery receipt.
  }
  private pump() {
    if (
      this.active ||
      this.closed ||
      !this.view().enabled ||
      this.saved.status !== 'ACCEPTED'
    )
      return;
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = null;
    }
    this.queue = this.queue.filter(
      (item) => item.expires > this.now() && item.generation === this.generation
    );
    this.bytes = this.queue.reduce((sum, item) => sum + item.bytes, 0);
    if (!this.queue.length) return;
    this.sentAt = this.sentAt.filter((time) => time > this.now() - 60_000);
    const wait =
      Math.max(
        this.nextSend,
        this.sentAt.length >= 30 ? this.sentAt[0] + 60_000 : 0
      ) - this.now();
    if (wait > 0) {
      this.timer = setTimeout(() => this.pump(), Math.min(wait, 30_000));
      this.timer.unref();
      return;
    }
    const item = this.queue.shift();
    this.bytes -= item.bytes;
    const controller = new AbortController();
    this.active = controller;
    this.sentAt.push(this.now());
    // One bounded request; caller never waits for the provider. Revocation can
    // invalidate even the microtask before it starts the transport.
    Promise.resolve()
      .then(() => {
        if (controller.signal.aborted || item.generation !== this.generation)
          return { status: 0 };
        return this.transport(
          this.config.endpoint,
          item.body,
          controller.signal
        );
      })
      .catch(() => ({ status: 0 }))
      .then((result) => {
        if (item.generation !== this.generation) return;
        if (result.status === 204) {
          this.failures = 0;
          this.nextSend = 0;
        } else {
          this.failures = Math.min(this.failures + 1, 6);
          const retry =
            'retryAfter' in result &&
            typeof result.retryAfter === 'number' &&
            Number.isFinite(result.retryAfter)
              ? result.retryAfter
              : 1;
          const seconds =
            result.status === 429 ? retry : 2 ** (this.failures - 1);
          this.nextSend =
            this.now() + Math.max(1, Math.min(60, seconds)) * 1000;
        }
      })
      .finally(() => {
        this.active = null;
        this.pump();
      });
  }
  private cancel() {
    this.generation++;
    this.queue = [];
    this.attempts.clear();
    this.lastAttempt = 0;
    this.bytes = 0;
    if (this.timer) clearTimeout(this.timer);
    this.timer = null;
    this.active?.abort();
    this.nextSend = 0;
    this.failures = 0;
    // Keep the dispatch budget across revoke/reaccept so toggling cannot evade it.
  }
  close() {
    this.closed = true;
    this.cancel();
  }
}
