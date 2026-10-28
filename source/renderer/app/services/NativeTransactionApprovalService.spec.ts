/** @jest-environment node */
import { nativeApprovalBindingDigest } from '../../../common/transactions/nativePlan';
import NativeTransactionApprovalService from './NativeTransactionApprovalService';
import { GenericApiError } from '../api/common/errors';
import { loadRecordedExchange } from '../../../../tests/jest/support/walletReplay';
import type { WalletTransactionApprovalMainResponse } from '../../../common/ipc/api';
import {
  bindNativeTransactionExecutor,
  requestNativeTransactionApproval,
  cancelNativeTransactionApproval,
} from '../ipc/nativeTransactionApproval';

jest.mock('../ipc/nativeTransactionApproval', () => ({
  bindNativeTransactionExecutor: jest.fn(() => jest.fn()),
  cancelNativeTransactionApproval: jest.fn(async () => ({
    status: 'accepted',
  })),
  commitNativeTransactionSubmission: jest.fn(async () => ({
    status: 'submission-authorized',
  })),
  reportWalletApprovalProgress: jest.fn(async () => ({ status: 'accepted' })),
  requestNativeTransactionApproval: jest.fn(),
}));
jest.mock('../../../common/transactions/nativePlan', () => ({
  ...(jest.requireActual('../../../common/transactions/nativePlan') as object),
  nativeApprovalBindingDigest: jest.fn(() => 'binding-digest'),
}));

const prepared = Object.freeze({
  walletId: 'wallet',
  network: {
    networkId: 0 as const,
    networkMagic: 42,
    genesisHash: '11'.repeat(32),
  },
  action: 'payment' as const,
  authorization: 'software' as const,
  items: Object.freeze([]),
  context: Object.freeze({ walletName: 'Savings', networkName: 'Preview' }),
});

test('requests a fresh approval for the same preparation after a wrong password', async () => {
  const digest = nativeApprovalBindingDigest(prepared);
  const request = requestNativeTransactionApproval as jest.MockedFunction<
    typeof requestNativeTransactionApproval
  >;
  request
    .mockResolvedValueOnce({
      status: 'accepted',
      requestId: 'first',
      bindingDigest: digest,
      result: { status: 'rejected', errorCode: 'wrong_encryption_passphrase' },
    })
    .mockResolvedValueOnce({
      status: 'submission-authorized',
      requestId: 'second',
      bindingDigest: digest,
      result: { status: 'submitted', transactionIds: ['transaction'] },
    });

  const service = new NativeTransactionApprovalService();
  await expect(
    service.request(
      prepared,
      async () => ({ status: 'submitted', transactionIds: ['unused'] }),
      new AbortController().signal
    )
  ).resolves.toEqual({ status: 'submitted', transactionIds: ['transaction'] });

  expect(request).toHaveBeenCalledTimes(2);
  expect(request.mock.calls[0][1]).toBe(prepared);
  expect(request.mock.calls[1][1]).toBe(prepared);
  service.dispose();
});

test('preserves explicit signing failures without mistaking a lost submission response for rejection', async () => {
  const request = requestNativeTransactionApproval as jest.MockedFunction<
    typeof requestNativeTransactionApproval
  >;
  request.mockImplementation(async (attemptId) => {
    const bind = bindNativeTransactionExecutor as jest.MockedFunction<
      typeof bindNativeTransactionExecutor
    >;
    const execute = bind.mock.calls[bind.mock.calls.length - 1][0];
    return {
      status: 'accepted',
      bindingDigest: nativeApprovalBindingDigest(prepared),
      result: await execute({
        type: 'execute',
        attemptId,
        requestId: 'request',
        bindingDigest: nativeApprovalBindingDigest(prepared),
      }),
    };
  });
  const service = new NativeTransactionApprovalService();
  const run = (
    execute: Parameters<NativeTransactionApprovalService['request']>[1]
  ) => service.request(prepared, execute, new AbortController().signal);
  try {
    await expect(
      run(async () => {
        throw Object.assign(new Error('device'), {
          code: 'TxSignError.UserDeclined',
        });
      })
    ).resolves.toEqual({
      status: 'rejected',
      errorCode: 'TxSignError.UserDeclined',
    });
    await expect(
      run(async () => {
        throw new Error('TxSignError.ProofGeneration');
      })
    ).resolves.toEqual({
      status: 'rejected',
      errorCode: 'TxSignError.ProofGeneration',
    });
    await expect(
      run(async () => {
        throw new Error('Action rejected by user on device.');
      })
    ).resolves.toEqual({ status: 'rejected', errorCode: 'failed' });
    await expect(
      run(async (_passphrase, _signal, markSubmitting) => {
        await markSubmitting();
        throw new Error('Connection closed before submission response');
      })
    ).resolves.toEqual({ status: 'submission-unknown', transactionIds: [] });
  } finally {
    service.dispose();
  }
});

test('reports a refusal the wallet gives as final after submission started as rejected, not unknown', async () => {
  const request = requestNativeTransactionApproval as jest.MockedFunction<
    typeof requestNativeTransactionApproval
  >;
  request.mockImplementation(async (attemptId) => {
    const bind = bindNativeTransactionExecutor as jest.MockedFunction<
      typeof bindNativeTransactionExecutor
    >;
    const execute = bind.mock.calls[bind.mock.calls.length - 1][0];
    return {
      status: 'accepted',
      bindingDigest: nativeApprovalBindingDigest(prepared),
      result: await execute({
        type: 'execute',
        attemptId,
        requestId: 'request',
        bindingDigest: nativeApprovalBindingDigest(prepared),
      }),
    };
  });
  const service = new NativeTransactionApprovalService();
  const run = (
    execute: Parameters<NativeTransactionApprovalService['request']>[1]
  ) => service.request(prepared, execute, new AbortController().signal);
  const refused = new GenericApiError(
    JSON.parse(loadRecordedExchange('cip30', 'submit-refused').response.body)
  );
  try {
    await expect(
      run(async (_passphrase, _signal, markSubmitting) => {
        await markSubmitting();
        throw refused;
      })
    ).resolves.toEqual({
      status: 'rejected',
      errorCode: 'withdrawal_not_possible_without_vote',
    });
    await expect(
      run(async (_passphrase, _signal, markSubmitting) => {
        await markSubmitting();
        throw Object.assign(new Error('Transaction submission failed'), {
          code: 'dapp_submission_failed',
        });
      })
    ).resolves.toEqual({
      status: 'rejected',
      errorCode: 'dapp_submission_failed',
    });
    await expect(
      run(async (_passphrase, _signal, markSubmitting) => {
        await markSubmitting();
        throw new Error('Connection closed before submission response');
      })
    ).resolves.toEqual({ status: 'submission-unknown', transactionIds: [] });
    await expect(
      run(async () => {
        throw refused;
      })
    ).resolves.toEqual({ status: 'rejected', errorCode: 'failed' });
  } finally {
    service.dispose();
  }
});

test('disposal cancels pending main approval and retains executor binding until its response settles', async () => {
  const request = requestNativeTransactionApproval as jest.MockedFunction<
    typeof requestNativeTransactionApproval
  >;
  let finish!: (value: WalletTransactionApprovalMainResponse) => void;
  request.mockImplementation(
    () =>
      new Promise((resolve) => {
        finish = resolve;
      })
  );
  const unbind = jest.fn();
  (bindNativeTransactionExecutor as jest.Mock).mockReturnValueOnce(unbind);
  const service = new NativeTransactionApprovalService();
  const pending = service.request(
    prepared,
    async () => ({ status: 'submitted', transactionIds: [] }),
    new AbortController().signal
  );
  const attemptId = request.mock.calls[0][0];
  service.dispose();
  expect(cancelNativeTransactionApproval).toHaveBeenCalledWith(attemptId);
  expect(unbind).not.toHaveBeenCalled();
  finish({
    status: 'accepted',
    bindingDigest: nativeApprovalBindingDigest(prepared),
    result: { status: 'rejected', errorCode: 'cancelled' },
  });
  await expect(pending).resolves.toEqual({
    status: 'rejected',
    errorCode: 'cancelled',
  });
  expect(unbind).toHaveBeenCalledTimes(1);
  await expect(
    service.request(
      prepared,
      async () => ({ status: 'submitted', transactionIds: [] }),
      new AbortController().signal
    )
  ).resolves.toEqual({ status: 'rejected', errorCode: 'cancelled' });
  expect(request).toHaveBeenCalledTimes(1);
});

test('disposal cannot release an already authorized submission before the backend result', async () => {
  const request = requestNativeTransactionApproval as jest.MockedFunction<
    typeof requestNativeTransactionApproval
  >;
  let finishSubmission!: () => void;
  const submission = new Promise<void>((resolve) => {
    finishSubmission = resolve;
  });
  let reportSubmitting!: () => void;
  const submitting = new Promise<void>((resolve) => {
    reportSubmitting = resolve;
  });
  const unbind = jest.fn();
  (bindNativeTransactionExecutor as jest.Mock).mockReturnValueOnce(unbind);
  request.mockImplementation(async (attemptId) => {
    const bind = bindNativeTransactionExecutor as jest.MockedFunction<
      typeof bindNativeTransactionExecutor
    >;
    const execute = bind.mock.calls[0][0];
    return {
      status: 'accepted',
      bindingDigest: nativeApprovalBindingDigest(prepared),
      result: await execute({
        type: 'execute',
        attemptId,
        requestId: 'submitting',
        bindingDigest: nativeApprovalBindingDigest(prepared),
      }),
    };
  });
  const service = new NativeTransactionApprovalService();
  let settled = false;
  const pending = service
    .request(
      prepared,
      async (_password, _signal, markSubmitting) => {
        await markSubmitting();
        reportSubmitting();
        await submission;
        return { status: 'submitted', transactionIds: ['actual-result'] };
      },
      new AbortController().signal
    )
    .then((result) => {
      settled = true;
      return result;
    });
  await submitting;
  service.dispose();
  await Promise.resolve();
  expect(settled).toBe(false);
  expect(unbind).not.toHaveBeenCalled();
  expect(cancelNativeTransactionApproval).toHaveBeenCalledWith(
    request.mock.calls[0][0]
  );
  finishSubmission();
  await expect(pending).resolves.toEqual({
    status: 'submitted',
    transactionIds: ['actual-result'],
  });
  expect(unbind).toHaveBeenCalledTimes(1);
});
