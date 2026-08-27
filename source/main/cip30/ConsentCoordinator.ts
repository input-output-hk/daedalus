import { randomUUID } from 'crypto';
import type { DappCip30Rejection } from '../../common/cip30/errors';
import type { DappConsentPresentation } from '../../common/ipc/api';

export const CONSENT_INACTIVITY_TIMEOUT_MS = 5 * 60 * 1000;

export type ConsentIdentity = Readonly<{
  guestWebContentsId: number;
  documentGeneration: number;
  origin: string;
  connectionId: string;
  walletId: string;
  routeEpoch: number;
  networkGenesis: string;
}>;

type ConsentPresentation = DappConsentPresentation extends infer Presentation
  ? Presentation extends DappConsentPresentation
    ? Omit<Presentation, 'requestId'>
    : never
  : never;
export type ConsentRequest<T> = Readonly<{
  identity: ConsentIdentity;
  presentation: ConsentPresentation;
  payload: unknown;
  declined: DappCip30Rejection;
  submission?: boolean;
  execute: (
    payload: unknown,
    signal: AbortSignal,
    passphrase?: string
  ) => Promise<T>;
}>;

type PendingConsent<T = unknown> = {
  readonly requestId: string;
  readonly identity: ConsentIdentity;
  readonly presentation: DappConsentPresentation;
  readonly payload: unknown;
  readonly declined: DappCip30Rejection;
  readonly submission: boolean;
  readonly execute: (
    payload: unknown,
    signal: AbortSignal,
    passphrase?: string
  ) => Promise<T>;
  readonly resolve: (value: T) => void;
  readonly reject: (reason: DappCip30Rejection) => void;
  readonly abort: AbortController;
  state: 'queued' | 'presented' | 'executing' | 'settled';
  staleRejection?: DappCip30Rejection;
  timer?: ReturnType<typeof setTimeout>;
};

export type ConsentCoordinatorOptions = Readonly<{
  present: (request: DappConsentPresentation) => Promise<void>;
  terminal: (requestId: string) => Promise<void>;
  setGuestHidden: (hidden: boolean) => void;
  inactivityTimeoutMs?: number;
}>;

const freezeValue = (value: unknown): unknown => {
  if (Array.isArray(value)) return Object.freeze(value.map(freezeValue));
  if (value && typeof value === 'object') {
    const prototype = Object.getPrototypeOf(value);
    if (prototype !== Object.prototype && prototype !== null)
      throw new Error('Consent payload must contain plain data');
    return Object.freeze(
      Object.keys(value).reduce<Record<string, unknown>>((copy, key) => {
        copy[key] = freezeValue((value as Record<string, unknown>)[key]);
        return copy;
      }, {})
    );
  }
  return value;
};

const isCip30Rejection = (value: unknown): value is DappCip30Rejection =>
  !!value &&
  typeof value === 'object' &&
  [
    'api-error',
    'paginate-error',
    'tx-sign-error',
    'data-sign-error',
    'tx-send-error',
    'cip103-submit-error',
  ].includes((value as { type?: string }).type || '') &&
  Object.prototype.hasOwnProperty.call(value, 'value');

export class ConsentCoordinator {
  private readonly queue: PendingConsent[] = [];
  private active?: PendingConsent;
  private readonly options: ConsentCoordinatorOptions;
  private readonly inactivityTimeoutMs: number;

  constructor(options: ConsentCoordinatorOptions) {
    this.options = options;
    this.inactivityTimeoutMs =
      options.inactivityTimeoutMs ?? CONSENT_INACTIVITY_TIMEOUT_MS;
  }

  request<T>(request: ConsentRequest<T>): Promise<T> {
    const requestId = randomUUID();
    const identity = freezeValue(request.identity) as ConsentIdentity;
    const presentation = freezeValue({
      ...request.presentation,
      requestId,
    }) as DappConsentPresentation;
    const payload = freezeValue(request.payload);
    return new Promise<T>((resolve, reject) => {
      this.queue.push({
        requestId,
        identity,
        presentation,
        payload,
        declined: freezeValue(request.declined) as DappCip30Rejection,
        submission: request.submission === true,
        execute: request.execute,
        resolve,
        reject,
        abort: new AbortController(),
        state: 'queued',
      });
      this.advance();
    });
  }

  decide(requestId: string, approved: boolean, passphrase?: string): void {
    const active = this.active;
    if (
      !active ||
      active.requestId !== requestId ||
      active.state !== 'presented'
    )
      return;
    this.clearTimer(active);
    if (!approved) {
      this.finish(active, active.declined);
      return;
    }

    active.state = 'executing';
    active
      .execute(active.payload, active.abort.signal, passphrase)
      .then((value) => {
        if (active.state !== 'settled')
          this.finish(active, active.staleRejection, value);
      })
      .catch((error) => {
        if (active.state !== 'settled')
          this.finish(
            active,
            active.staleRejection ??
              (isCip30Rejection(error) ? error : active.declined)
          );
      });
  }

  activity(requestId?: string): void {
    const active = this.active;
    if (
      active?.state === 'presented' &&
      (requestId === undefined || active.requestId === requestId)
    )
      this.startTimer(active);
  }

  cancel(
    matches: (identity: ConsentIdentity) => boolean = () => true,
    rejection?: DappCip30Rejection
  ): void {
    for (let index = this.queue.length - 1; index >= 0; index -= 1) {
      const pending = this.queue[index];
      if (matches(pending.identity)) {
        this.queue.splice(index, 1);
        pending.state = 'settled';
        pending.reject(rejection ?? pending.declined);
      }
    }

    const active = this.active;
    if (!active || !matches(active.identity)) return;
    const reason = rejection ?? active.declined;
    if (active.state === 'executing' && active.submission) {
      active.staleRejection = reason;
      return;
    }
    active.abort.abort();
    this.finish(active, reason);
  }

  private advance(): void {
    if (this.active) return;
    const next = this.queue.shift();
    if (!next) {
      this.options.setGuestHidden(false);
      return;
    }
    this.active = next;
    next.state = 'presented';
    this.options.setGuestHidden(true);
    this.startTimer(next);
    this.options.present(next.presentation).catch(() => {
      if (this.active === next && next.state === 'presented')
        this.finish(next, next.declined);
    });
  }

  private startTimer(pending: PendingConsent): void {
    this.clearTimer(pending);
    pending.timer = setTimeout(
      () => this.finish(pending, pending.declined),
      this.inactivityTimeoutMs
    );
  }

  private clearTimer(pending: PendingConsent): void {
    clearTimeout(pending.timer);
    pending.timer = undefined;
  }

  private finish<T>(
    pending: PendingConsent<T>,
    rejection?: DappCip30Rejection,
    value?: T
  ): void {
    if (pending.state === 'settled') return;
    pending.state = 'settled';
    this.clearTimer(pending);
    if (this.active === pending) this.active = undefined;
    this.options.terminal(pending.requestId).catch(() => undefined);
    if (rejection) pending.reject(rejection);
    else pending.resolve(value as T);
    this.advance();
  }
}
