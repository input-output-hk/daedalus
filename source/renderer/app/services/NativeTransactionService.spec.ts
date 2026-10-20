/** @jest-environment node */
import NativeTransactionService from './NativeTransactionService';
import NativeTransactionApprovalService from './NativeTransactionApprovalService';
import TransactionsStore from '../stores/TransactionsStore';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';
import type { NativeApprovalResult } from '../../../common/transactions/nativePlan';

const prepared = {
  walletId: 'wallet',
  network: {
    networkId: 0 as const,
    networkMagic: 42,
    genesisHash: '11'.repeat(32),
  },
  action: 'payment' as const,
  authorization: 'software' as const,
  items: [],
  context: { walletName: 'Savings', networkName: 'Preview' },
};
const submitted: NativeApprovalResult = {
  status: 'submitted',
  transactionIds: ['transaction'],
};
const cancelled: NativeApprovalResult = {
  status: 'rejected',
  errorCode: 'cancelled',
};
const deferred = <T = void>() => {
  let resolve!: (value: T) => void;
  const promise = new Promise<T>((complete) => {
    resolve = complete;
  });
  return { promise, resolve };
};
const queue = () => {
  // No backend methods are called by the real wallet lease queue.
  const api = { ada: {} } as unknown as Api;
  return new TransactionsStore(api, {} as ActionsMap, {} as AnalyticsTracker);
};
const approvalBoundary = () => {
  const request = jest.fn<
    Promise<NativeApprovalResult>,
    Parameters<NativeTransactionApprovalService['request']>
  >(async (_prepared, execute) =>
    execute(
      'password',
      new AbortController().signal,
      async () => undefined,
      'approval'
    )
  );
  const dispose = jest.fn();
  // The approval IPC boundary is controlled; wallet serialization is the production queue.
  const approval = {
    request,
    dispose,
  } as unknown as NativeTransactionApprovalService;
  return { request, dispose, approval };
};
const operation = (walletId = 'wallet') => ({
  walletId,
  ownerSignal: new AbortController().signal,
  prepare: jest.fn(async () => ({ ...prepared, walletId })),
  execute: jest.fn<Promise<NativeApprovalResult>, []>(async () => submitted),
});

test('holds the same wallet through preparation, consent, signing and submission while another wallet progresses', async () => {
  const transactions = queue();
  const boundary = approvalBoundary();
  const preparing = deferred();
  const allowPreparation = deferred();
  const approving = deferred();
  const allowApproval = deferred();
  const signing = deferred();
  const allowSigning = deferred();
  const submitting = deferred();
  const allowSubmission = deferred();
  const onExecute = jest.fn();
  let firstApproval = true;
  boundary.request.mockImplementation(async (reviewed, execute) => {
    if (reviewed.walletId === 'wallet' && firstApproval) {
      firstApproval = false;
      approving.resolve();
      await allowApproval.promise;
      return execute(
        'password',
        new AbortController().signal,
        async () => undefined,
        'approval-a'
      );
    }
    return execute(
      'password',
      new AbortController().signal,
      async () => undefined,
      'approval'
    );
  });
  const service = new NativeTransactionService(
    transactions.withWalletSendLock,
    boundary.approval,
    onExecute
  );
  let backendInput = 'unspent';
  const first = service.run({
    ...operation(),
    prepare: async () => {
      preparing.resolve();
      await allowPreparation.promise;
      return prepared;
    },
    execute: async () => {
      signing.resolve();
      await allowSigning.promise;
      submitting.resolve();
      await allowSubmission.promise;
      backendInput = 'spent';
      return submitted;
    },
  });
  await preparing.promise;
  const next = operation();
  const second = service.run(next);
  let dappContext: string | undefined;
  const dapp = transactions.withWalletSendLock('wallet', async () => {
    dappContext = backendInput;
  });
  await service.run(operation('another-wallet'));
  expect(next.prepare).not.toHaveBeenCalled();
  expect(dappContext).toBeUndefined();
  onExecute.mockClear();
  allowPreparation.resolve();
  await approving.promise;
  expect(next.prepare).not.toHaveBeenCalled();
  expect(onExecute).not.toHaveBeenCalled();
  allowApproval.resolve();
  await signing.promise;
  expect(next.prepare).not.toHaveBeenCalled();
  expect(onExecute).toHaveBeenCalledWith('approval-a', undefined);
  allowSigning.resolve();
  await submitting.promise;
  expect(next.prepare).not.toHaveBeenCalled();
  expect(dappContext).toBeUndefined();
  allowSubmission.resolve();
  await expect(first).resolves.toEqual(submitted);
  await expect(second).resolves.toEqual(submitted);
  await dapp;
  expect(dappContext).toBe('spent');
});

test.each([
  'prepare-error',
  'approval-error',
  'approval-rejected',
  'execution-error',
  'execution-rejected',
  'submission-unknown',
] as const)('allows the next same-wallet operation after %s', async (phase) => {
  const transactions = queue();
  const boundary = approvalBoundary();
  const service = new NativeTransactionService(
    transactions.withWalletSendLock,
    boundary.approval
  );
  const first = operation();
  if (phase === 'prepare-error')
    first.prepare.mockRejectedValueOnce(new Error('prepare failed'));
  if (phase === 'approval-error')
    boundary.request.mockRejectedValueOnce(new Error('approval failed'));
  if (phase === 'approval-rejected')
    boundary.request.mockResolvedValueOnce(cancelled);
  if (phase === 'execution-error')
    first.execute.mockRejectedValueOnce(new Error('device failed'));
  if (phase === 'execution-rejected')
    first.execute.mockResolvedValueOnce(cancelled);
  if (phase === 'submission-unknown')
    first.execute.mockResolvedValueOnce({
      status: 'submission-unknown',
      transactionIds: ['transaction'],
    });
  const outcome = service.run(first).catch((error: unknown) => error);
  const next = operation();
  const following = service.run(next);
  await outcome;
  await expect(following).resolves.toEqual(submitted);
  expect(next.prepare).toHaveBeenCalledTimes(1);
});

test.each(['abort', 'dispose'] as const)(
  'skips queued preparation after owner %s and does not orphan the lease',
  async (reason) => {
    const transactions = queue();
    const held = await transactions.acquireWalletSendLock('wallet');
    const boundary = approvalBoundary();
    const service = new NativeTransactionService(
      transactions.withWalletSendLock,
      boundary.approval
    );
    const owner = new AbortController();
    const pending = { ...operation(), ownerSignal: owner.signal };
    const outcome = service.run(pending);
    if (reason === 'abort') owner.abort();
    else service.dispose();
    held.release();
    await expect(outcome).resolves.toEqual(cancelled);
    expect(pending.prepare).not.toHaveBeenCalled();
    expect(boundary.request).not.toHaveBeenCalled();
    const nextService = new NativeTransactionService(
      transactions.withWalletSendLock,
      approvalBoundary().approval
    );
    await expect(nextService.run(operation())).resolves.toEqual(submitted);
    await expect(service.run(operation())).resolves.toEqual(
      reason === 'dispose' ? cancelled : submitted
    );
    expect(owner.signal.aborted).toBe(reason === 'abort');
  }
);

test.each(['abort', 'dispose', 'wrong-wallet'] as const)(
  'rejects a prepared transaction after %s before consent or device work',
  async (reason) => {
    const transactions = queue();
    const boundary = approvalBoundary();
    const service = new NativeTransactionService(
      transactions.withWalletSendLock,
      boundary.approval
    );
    const started = deferred();
    const finish = deferred();
    const owner = new AbortController();
    const first = operation();
    const outcome = service.run({
      ...first,
      ownerSignal: owner.signal,
      prepare: async () => {
        started.resolve();
        await finish.promise;
        return {
          ...prepared,
          walletId: reason === 'wrong-wallet' ? 'other-wallet' : 'wallet',
        };
      },
    });
    await started.promise;
    if (reason === 'abort') owner.abort();
    if (reason === 'dispose') service.dispose();
    finish.resolve();
    await expect(outcome).resolves.toEqual(cancelled);
    expect(boundary.request).not.toHaveBeenCalled();
    expect(first.execute).not.toHaveBeenCalled();
    const nextService = new NativeTransactionService(
      transactions.withWalletSendLock,
      approvalBoundary().approval
    );
    await expect(nextService.run(operation())).resolves.toEqual(submitted);
  }
);

test('retains the authoritative preparation and wallet lease during password retry', async () => {
  const transactions = queue();
  const boundary = approvalBoundary();
  const retrying = deferred();
  const allowRetry = deferred();
  boundary.request.mockImplementationOnce(async (_prepared, execute) => {
    const first = await execute(
      'wrong',
      new AbortController().signal,
      async () => undefined,
      'first'
    );
    expect(first).toEqual({
      status: 'rejected',
      errorCode: 'wrong_encryption_passphrase',
    });
    retrying.resolve();
    await allowRetry.promise;
    return execute(
      'correct',
      new AbortController().signal,
      async () => undefined,
      'retry'
    );
  });
  const service = new NativeTransactionService(
    transactions.withWalletSendLock,
    boundary.approval
  );
  const first = operation();
  first.execute.mockResolvedValueOnce({
    status: 'rejected',
    errorCode: 'wrong_encryption_passphrase',
  });
  const outcome = service.run(first);
  await retrying.promise;
  const next = operation();
  const following = service.run(next);
  await Promise.resolve();
  expect(next.prepare).not.toHaveBeenCalled();
  allowRetry.resolve();
  await expect(outcome).resolves.toEqual(submitted);
  await expect(following).resolves.toEqual(submitted);
  expect(first.prepare).toHaveBeenCalledTimes(1);
  expect(first.execute).toHaveBeenCalledTimes(2);
});
