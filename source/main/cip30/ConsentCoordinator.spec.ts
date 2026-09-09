import type { DappCip30Rejection } from '../../common/cip30/errors';
import type {
  NativeTransactionPresentation,
  WalletApprovalPresentation,
  WalletApprovalResult,
} from '../../common/ipc/api';
import { ConsentCoordinator, ConsentRequest } from './ConsentCoordinator';
import type { NativeApprovalResult } from '../../common/transactions/nativePlan';

const declined: DappCip30Rejection = {
  type: 'api-error',
  value: { code: -3, info: 'Refused' },
};
const walletId = 'aa'.repeat(20);
const identity = {
  kind: 'dapp' as const,
  guestWebContentsId: 7,
  documentGeneration: 2,
  origin: 'https://example.test',
  connectionId: 'connection',
  walletId,
  routeEpoch: 3,
  networkGenesis: 'genesis',
};
const presentation = {
  kind: 'connection' as const,
  origin: identity.origin,
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['connection', 'read'],
  extensions: [95],
};

const setup = (timeout = 300_000, holdNativeResult = false) => {
  const presented: WalletApprovalPresentation[] = [];
  const terminal: string[] = [];
  const terminalResults: Array<WalletApprovalResult | undefined> = [];
  const hidden: boolean[] = [];
  const progress: unknown[] = [];
  let dismissResult = (): void => undefined;
  const terminalGate = new Promise<void>((resolve) => {
    dismissResult = resolve;
  });
  const coordinator = new ConsentCoordinator({
    present: async (request) => {
      presented.push(request);
    },
    progress: async (...value) => {
      progress.push(value);
    },
    terminal: async (requestId, result) => {
      terminal.push(requestId);
      terminalResults.push(result);
      if (holdNativeResult && result) await terminalGate;
    },
    setGuestHidden: (value) => hidden.push(value),
    inactivityTimeoutMs: timeout,
  });
  return {
    coordinator,
    presented,
    terminal,
    terminalResults,
    dismissResult,
    hidden,
    progress,
  };
};

const request = <T>(
  coordinator: ConsentCoordinator,
  execute: ConsentRequest<T>['execute'],
  options: { payload?: unknown; submission?: boolean } = {}
) =>
  coordinator.request({
    identity,
    presentation,
    payload: options.payload ?? { bytes: 'aabb' },
    declined,
    submission: options.submission,
    execute,
  });

const nativeIdentity = {
  kind: 'native' as const,
  trustedWebContentsId: 1,
  attemptId: 'native-attempt',
  walletId,
  networkGenesis: 'genesis',
};
const nativePresentation: Omit<
  NativeTransactionPresentation,
  'requestId' | 'walletId'
> = {
  kind: 'native-transaction',
  attemptId: nativeIdentity.attemptId,
  walletName: 'Wallet',
  networkName: 'Preview',
  action: 'payment',
  authorization: { kind: 'hardware', vendor: 'ledger' },
  collection: 'single',
  acknowledgements: [],
  items: [],
};
const nativeDeclined = {
  type: 'native-transaction-error' as const,
  value: { code: 'user_declined', info: 'Transaction was rejected' },
};
const transactionPresentation = ({
  kind: 'transaction-sign' as const,
  origin: identity.origin,
  walletName: 'Wallet',
  networkName: 'Preview',
  scopes: ['transaction-signing'],
  extensions: [],
  authorization: { kind: 'software' as const },
  review: { transactionId: '44'.repeat(32) },
} as unknown) as ConsentRequest<string>['presentation'];

describe('ConsentCoordinator', () => {
  afterEach(() => jest.useRealTimers());

  it('queues requests FIFO and executes immutable broker-owned payloads', async () => {
    const { coordinator, presented, terminal, hidden } = setup();
    const payload = { bytes: 'aabb', nested: ['fixed'] };
    const firstExecute = jest.fn(async (value) => value);
    const secondExecute = jest.fn(async () => 'second');
    const first = request(coordinator, firstExecute, { payload });
    const second = request(coordinator, secondExecute);

    expect(presented).toHaveLength(1);
    expect(Object.isFrozen(presented[0])).toBe(true);
    payload.bytes = 'replaced';
    coordinator.decide(presented[0].requestId, true);
    await expect(first).resolves.toEqual({ bytes: 'aabb', nested: ['fixed'] });
    expect(firstExecute).toHaveBeenCalledWith(
      { bytes: 'aabb', nested: ['fixed'] },
      expect.any(AbortSignal),
      undefined,
      expect.objectContaining({
        requestId: presented[0].requestId,
        reportProgress: expect.any(Function),
      })
    );
    expect(terminal).toEqual([presented[0].requestId]);
    expect(presented).toHaveLength(2);

    coordinator.decide(presented[1].requestId, true);
    await expect(second).resolves.toBe('second');
    expect(hidden).toEqual([true, true, false]);
  });

  it('pins the presentation wallet to the trusted consent identity', async () => {
    const { coordinator, presented } = setup();
    const pending = coordinator.request({
      identity,
      presentation: {
        ...presentation,
        walletId: 'bb'.repeat(20),
      } as ConsentRequest<string>['presentation'],
      payload: {},
      declined,
      execute: async () => 'unused',
    });

    expect(presented[0].walletId).toBe(walletId);
    expect(Object.isFrozen(presented[0])).toBe(true);
    coordinator.decide(presented[0].requestId, false);
    await expect(pending).rejects.toEqual(declined);
  });

  it('expires after five minutes of inactivity and activity resets the timer', async () => {
    jest.useFakeTimers();
    const { coordinator, presented } = setup(300_000);
    const pending = request(coordinator, async () => 'unused');

    jest.advanceTimersByTime(299_000);
    coordinator.activity(presented[0].requestId);
    jest.advanceTimersByTime(299_000);
    coordinator.decide('stale-request', true);
    jest.advanceTimersByTime(1_001);

    await expect(pending).rejects.toEqual(declined);
  });

  it('rejects refusal once and ignores late, duplicate, and replayed decisions', async () => {
    const { coordinator, presented, terminal } = setup();
    const execute = jest.fn(async () => 'unused');
    const pending = request(coordinator, execute);
    const requestId = presented[0].requestId;

    coordinator.decide(requestId, false);
    coordinator.decide(requestId, true);
    coordinator.decide(requestId, false);

    await expect(pending).rejects.toEqual(declined);
    expect(execute).not.toHaveBeenCalled();
    expect(terminal).toEqual([requestId]);
  });

  it('cancels matching work with the exact lifecycle rejection', async () => {
    const { coordinator, presented } = setup();
    let signal: AbortSignal | undefined;
    const pending = request(coordinator, async (_payload, nextSignal) => {
      signal = nextSignal;
      return new Promise(() => undefined);
    });
    coordinator.decide(presented[0].requestId, true);
    const accountChanged: DappCip30Rejection = {
      type: 'api-error',
      value: { code: -4, info: 'Account changed' },
    };

    coordinator.cancel(
      (candidate) => candidate.walletId === walletId,
      accountChanged
    );

    await expect(pending).rejects.toEqual(accountChanged);
    expect(signal?.aborted).toBe(true);
  });

  it('continues an authorized submission but suppresses its stale result', async () => {
    const { coordinator, presented } = setup();
    let finish: (value: string) => void = () => undefined;
    const execute = jest.fn(
      async () => new Promise<string>((resolve) => (finish = resolve))
    );
    const pending = request(coordinator, execute, { submission: true });
    coordinator.decide(presented[0].requestId, true);

    coordinator.cancel();
    expect(execute).toHaveBeenCalledTimes(1);
    finish('transaction-id');

    await expect(pending).rejects.toEqual(declined);
  });

  it('correlates progress to the executing request and valid item', async () => {
    const { coordinator, presented, progress } = setup();
    let finish: (value: string) => void = () => undefined;
    const pending = request(
      coordinator,
      async (_payload, _signal, _passphrase, context) => {
        context.reportProgress('signing', 0);
        return new Promise<string>((resolve) => {
          finish = resolve;
        });
      }
    );
    const requestId = presented[0].requestId;
    coordinator.decide(requestId, true);
    await Promise.resolve();

    expect(progress).toContainEqual([requestId, 'signing', 0, false]);
    expect(coordinator.reportProgress('stale', 'submitting', 0)).toBe('stale');
    expect(coordinator.reportProgress(requestId, 'submitting', 1)).toBe(
      'stale'
    );
    expect(coordinator.reportProgress(requestId, 'submitting', 0)).toBe(
      'accepted'
    );
    finish('done');
    await expect(pending).resolves.toBe('done');
  });

  it('passes transient passphrase once and preserves typed execution errors', async () => {
    const { coordinator, presented } = setup();
    const typedError = Object.assign(new Error('Proof generation failed'), {
      type: 'data-sign-error' as const,
      value: { code: 1, info: 'Proof generation failed' },
    });
    const execute = jest.fn(async () => {
      throw typedError;
    });
    const pending = request(coordinator, execute);
    coordinator.decide(presented[0].requestId, true, 'secret');
    await expect(pending).rejects.toBe(typedError);
    expect(execute).toHaveBeenCalledWith(
      { bytes: 'aabb' },
      expect.any(AbortSignal),
      'secret',
      expect.objectContaining({
        requestId: presented[0].requestId,
        reportProgress: expect.any(Function),
      })
    );
  });

  it('keeps a successful native result after authorized-submission cancellation', async () => {
    const { coordinator, presented, terminalResults } = setup();
    let finishExecution = (_value: NativeApprovalResult): void => undefined;
    const execution = new Promise<NativeApprovalResult>((resolve) => {
      finishExecution = resolve;
    });
    const pending = coordinator.request({
      identity: nativeIdentity,
      presentation: nativePresentation,
      payload: {},
      declined: nativeDeclined,
      submission: true,
      execute: async () => execution,
    });
    coordinator.decide(presented[0].requestId, true);
    coordinator.cancel();
    finishExecution({
      status: 'submitted',
      transactionIds: ['33'.repeat(32)],
    });
    await expect(pending).resolves.toEqual({
      status: 'submitted',
      transactionIds: ['33'.repeat(32)],
    });
    expect(terminalResults).toEqual([
      {
        status: 'submitted',
        transactionIds: ['33'.repeat(32)],
      },
    ]);
  });

  it('settles native execution immediately but gates the next review on result dismissal', async () => {
    const { coordinator, presented, terminalResults, dismissResult } = setup(
      300_000,
      true
    );
    const submitted: NativeApprovalResult = {
      status: 'submitted',
      transactionIds: ['11'.repeat(32)],
    };
    const first = coordinator.request({
      identity: nativeIdentity,
      presentation: nativePresentation,
      payload: {},
      declined: nativeDeclined,
      execute: async () => submitted,
    });
    const second = request(coordinator, async () => 'next');

    coordinator.decide(presented[0].requestId, true);
    await expect(first).resolves.toEqual(submitted);
    expect(terminalResults).toEqual([submitted]);
    expect(presented).toHaveLength(1);

    dismissResult();
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(presented).toHaveLength(2);
    coordinator.decide(presented[1].requestId, true);
    await expect(second).resolves.toBe('next');
  });
  it('reports signed transaction ids without changing the public result and gates the queue', async () => {
    const { coordinator, presented, terminalResults, dismissResult } = setup(
      300_000,
      true
    );
    const signed = coordinator.request({
      identity,
      presentation: transactionPresentation,
      payload: {},
      declined,
      execute: async () => 'public-witness',
    });
    const next = request(coordinator, async () => 'next');

    coordinator.decide(presented[0].requestId, true);
    await expect(signed).resolves.toBe('public-witness');
    expect(terminalResults).toEqual([
      { status: 'signed', transactionIds: ['44'.repeat(32)] },
    ]);
    expect(presented).toHaveLength(1);

    dismissResult();
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(presented).toHaveLength(2);
    coordinator.decide(presented[1].requestId, true);
    await expect(next).resolves.toBe('next');
  });

  it('retains a reported uncertain submission while preserving its public return', async () => {
    const { coordinator, presented, terminalResults } = setup();
    const submitted = coordinator.request({
      identity,
      presentation: {
        ...transactionPresentation,
        kind: 'transaction-submit',
        authorization: { kind: 'none' },
      } as ConsentRequest<string>['presentation'],
      payload: {},
      declined,
      submission: true,
      execute: async (_payload, _signal, _passphrase, context) => {
        context.reportResult({
          status: 'submission-unknown',
          transactionIds: ['44'.repeat(32)],
        });
        return '44'.repeat(32);
      },
    });

    coordinator.decide(presented[0].requestId, true);
    await expect(submitted).resolves.toBe('44'.repeat(32));
    expect(terminalResults).toEqual([
      {
        status: 'submission-unknown',
        transactionIds: ['44'.repeat(32)],
      },
    ]);
  });

  it('distinguishes refusal and unexpected native failure without blocking passphrase retry', async () => {
    const refusal = setup();
    const refused = refusal.coordinator.request({
      identity: nativeIdentity,
      presentation: nativePresentation,
      payload: {},
      declined: nativeDeclined,
      execute: async () => ({
        status: 'submitted' as const,
        transactionIds: ['22'.repeat(32)],
      }),
    });
    refusal.coordinator.decide(refusal.presented[0].requestId, false);
    await expect(refused).rejects.toEqual(nativeDeclined);
    expect(refusal.terminalResults).toEqual([
      { status: 'rejected', errorCode: 'user_declined' },
    ]);

    const failed = setup();
    const unexpected = failed.coordinator.request({
      identity: nativeIdentity,
      presentation: nativePresentation,
      payload: {},
      declined: nativeDeclined,
      execute: async () => {
        throw new Error('raw device failure');
      },
    });
    failed.coordinator.decide(failed.presented[0].requestId, true);
    await expect(unexpected).rejects.toMatchObject({
      type: 'native-transaction-error',
      value: { code: 'failed' },
    });
    expect(failed.terminalResults).toEqual([
      { status: 'rejected', errorCode: 'failed' },
    ]);

    const retry = setup();
    const wrongPassphrase = retry.coordinator.request({
      identity: nativeIdentity,
      presentation: nativePresentation,
      payload: {},
      declined: nativeDeclined,
      execute: async () => ({
        status: 'rejected' as const,
        errorCode: 'wrong_encryption_passphrase',
      }),
    });
    const next = request(retry.coordinator, async () => 'retry');
    retry.coordinator.decide(retry.presented[0].requestId, true, 'wrong');
    await expect(wrongPassphrase).resolves.toEqual({
      status: 'rejected',
      errorCode: 'wrong_encryption_passphrase',
    });
    await Promise.resolve();
    expect(retry.terminalResults).toEqual([undefined]);
    expect(retry.presented).toHaveLength(2);
    retry.coordinator.decide(retry.presented[1].requestId, true);
    await expect(next).resolves.toBe('retry');
  });
});
