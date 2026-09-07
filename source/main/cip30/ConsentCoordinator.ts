import { randomUUID } from 'crypto';
import type { DappCip30Rejection } from '../../common/cip30/errors';
import type {
  WalletApprovalPresentation,
  WalletApprovalProgressPhase,
} from '../../common/ipc/api';

export type NativeTransactionFailure = Readonly<{
  type: 'native-transaction-error';
  value: Readonly<{ code: string; info: string }>;
}>;
export type ConsentFailure = DappCip30Rejection | NativeTransactionFailure;
export const CONSENT_INACTIVITY_TIMEOUT_MS = 5 * 60 * 1000;

export type ConsentIdentity =
  | Readonly<{
      kind: 'dapp';
      guestWebContentsId: number;
      documentGeneration: number;
      origin: string;
      connectionId: string;
      walletId: string;
      routeEpoch: number;
      networkGenesis: string;
    }>
  | Readonly<{
      kind: 'native';
      trustedWebContentsId: number;
      attemptId: string;
      walletId: string;
      networkGenesis: string;
    }>;

type ConsentPresentation = WalletApprovalPresentation extends infer Presentation
  ? Presentation extends WalletApprovalPresentation
    ? Omit<Presentation, 'requestId'>
    : never
  : never;
export type ConsentRequest<T> = Readonly<{
  identity: ConsentIdentity;
  presentation: ConsentPresentation;
  payload: unknown;
  declined: ConsentFailure;
  submission?: boolean;
  onCreated?: (requestId: string) => void;
  execute: (
    payload: unknown,
    signal: AbortSignal,
    passphrase: string | undefined,
    context: Readonly<{
      requestId: string;
      reportProgress: (
        phase: WalletApprovalProgressPhase,
        itemIndex?: number
      ) => void;
    }>
  ) => Promise<T>;
}>;

type PendingConsent<T = unknown> = {
  readonly requestId: string;
  readonly identity: ConsentIdentity;
  readonly presentation: WalletApprovalPresentation;
  readonly payload: unknown;
  readonly declined: ConsentFailure;
  submission: boolean;
  readonly execute: (
    payload: unknown,
    signal: AbortSignal,
    passphrase: string | undefined,
    context: Readonly<{
      requestId: string;
      reportProgress: (
        phase: WalletApprovalProgressPhase,
        itemIndex?: number
      ) => void;
    }>
  ) => Promise<T>;
  readonly resolve: (value: T) => void;
  readonly reject: (reason: ConsentFailure) => void;
  readonly abort: AbortController;
  state: 'queued' | 'presented' | 'executing' | 'settled';
  staleRejection?: ConsentFailure;
  timer?: ReturnType<typeof setTimeout>;
};

export type ConsentCoordinatorOptions = Readonly<{
  present: (request: WalletApprovalPresentation) => Promise<void>;
  progress: (
    requestId: string,
    phase: WalletApprovalProgressPhase,
    itemIndex: number | undefined,
    submissionAuthorized: boolean
  ) => Promise<void>;
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

const isConsentFailure = (value: unknown): value is ConsentFailure => {
  if (
    !value ||
    typeof value !== 'object' ||
    !('type' in value) ||
    typeof value.type !== 'string'
  )
    return false;
  return (
    [
      'api-error',
      'paginate-error',
      'tx-sign-error',
      'data-sign-error',
      'tx-send-error',
      'cip103-submit-error',
      'native-transaction-error',
    ].includes(value.type) && 'value' in value
  );
};

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
    request.onCreated?.(requestId);
    const identity = freezeValue(request.identity) as ConsentIdentity;
    const presentation = freezeValue({
      ...request.presentation,
      requestId,
    }) as WalletApprovalPresentation;
    const payload = freezeValue(request.payload);
    return new Promise<T>((resolve, reject) => {
      this.queue.push({
        requestId,
        identity,
        presentation,
        payload,
        declined: freezeValue(request.declined) as ConsentFailure,
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
    const executionContext = Object.freeze({
      requestId: active.requestId,
      reportProgress: (
        phase: WalletApprovalProgressPhase,
        itemIndex?: number
      ) => {
        if (active.state !== 'executing') return;
        this.options
          .progress(active.requestId, phase, itemIndex, active.submission)
          .catch(() => undefined);
      },
    });
    active
      .execute(
        active.payload,
        active.abort.signal,
        passphrase,
        executionContext
      )
      .then((value) => {
        if (active.state !== 'settled')
          this.finish(active, active.staleRejection, value);
      })
      .catch((error) => {
        if (active.state !== 'settled')
          this.finish(
            active,
            isConsentFailure(error) ? error : active.declined
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
  reportProgress(
    requestId: string,
    phase: WalletApprovalProgressPhase,
    itemIndex?: number
  ): 'accepted' | 'stale' | 'submission-authorized' {
    const active = this.active;
    if (
      !active ||
      active.requestId !== requestId ||
      active.state !== 'executing' ||
      !this.validItemIndex(active.presentation, itemIndex)
    )
      return 'stale';
    this.options
      .progress(requestId, phase, itemIndex, active.submission)
      .catch(() => undefined);
    return active.submission ? 'submission-authorized' : 'accepted';
  }
  markSubmissionAuthorized(
    requestId: string
  ): 'accepted' | 'stale' | 'submission-authorized' {
    const active = this.active;
    if (
      !active ||
      active.requestId !== requestId ||
      active.state !== 'executing' ||
      active.abort.signal.aborted
    )
      return 'stale';
    active.submission = true;
    return 'submission-authorized';
  }

  cancel(
    matches: (identity: ConsentIdentity) => boolean = () => true,
    rejection?: ConsentFailure
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

  private validItemIndex(
    presentation: WalletApprovalPresentation,
    itemIndex?: number
  ): boolean {
    if (itemIndex === undefined) return true;
    if (
      presentation.kind === 'batch-sign' ||
      presentation.kind === 'batch-submit'
    )
      return itemIndex >= 0 && itemIndex < presentation.review.items.length;
    if (presentation.kind === 'native-transaction')
      return itemIndex >= 0 && itemIndex < presentation.items.length;
    return itemIndex === 0;
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
    this.options.setGuestHidden(next.identity.kind === 'dapp');
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
    rejection?: ConsentFailure,
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
