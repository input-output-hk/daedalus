/** @jest-environment node */
import { nativeApprovalBindingDigest } from '../../../common/transactions/nativePlan';
import NativeTransactionApprovalService from './NativeTransactionApprovalService';
import { requestNativeTransactionApproval } from '../ipc/nativeTransactionApproval';

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
