import {
  reconcileTransactionContext,
  TransactionContextSnapshot,
} from '../../common/cardano/transactionContext';
import {
  DappTransactionContextService,
  DappTransactionContextServiceError,
} from './DappTransactionContextService';

jest.mock('../../common/cardano/transactionContext', () => ({
  reconcileTransactionContext: jest.fn(),
}));

const walletId = 'ab'.repeat(20);
const transaction = `84a30081825820${'11'.repeat(32)}0001800200a0f5f6`;
const binding = {
  walletId,
  network: {
    networkId: 0 as const,
    networkMagic: 42,
    genesisHash: '01'.repeat(32),
  },
  generation: 7,
};
const snapshot = Object.freeze({
  contextDigest: '02'.repeat(32),
}) as TransactionContextSnapshot;

beforeEach(() => {
  jest.resetAllMocks();
  (reconcileTransactionContext as jest.Mock).mockReturnValue(snapshot);
});

test('posts only exact transaction bytes and main-owned identity', async () => {
  const transport = jest.fn().mockResolvedValue({ response: true });
  const service = new DappTransactionContextService(transport, () => binding);
  await expect(service.capture(binding, [transaction])).resolves.toBe(snapshot);
  expect(transport).toHaveBeenCalledWith(
    `/v2/wallets/${walletId}/transaction-context`,
    {
      revision: 1,
      network: {
        network_id: 0,
        network_magic: 42,
        genesis_hash: binding.network.genesisHash,
      },
      transactions: [transaction],
    }
  );
  expect(reconcileTransactionContext).toHaveBeenCalledWith(
    { response: true },
    {
      walletId,
      network: binding.network,
      transactions: [transaction],
    }
  );
});

test('rejects stale route binding before and after backend capture', async () => {
  const transport = jest.fn().mockResolvedValue({});
  const stale = { ...binding, generation: binding.generation + 1 };
  await expect(
    new DappTransactionContextService(transport, () => stale).capture(binding, [
      transaction,
    ])
  ).rejects.toMatchObject({ failure: 'account_changed' });
  expect(transport).not.toHaveBeenCalled();

  let calls = 0;
  const service = new DappTransactionContextService(transport, () =>
    calls++ ? stale : binding
  );
  await expect(service.capture(binding, [transaction])).rejects.toMatchObject({
    failure: 'account_changed',
  });
  expect(reconcileTransactionContext).not.toHaveBeenCalled();
});

test.each([
  [400, 'dapp_invalid_request', 'invalid_request'],
  [400, 'dapp_context_conflict', 'context_conflict'],
  [409, 'dapp_account_changed', 'account_changed'],
  [503, 'dapp_context_unavailable', 'context_unavailable'],
  [500, 'dapp_internal_error', 'internal_error'],
  [400, 'attacker_detail', 'internal_error'],
])(
  'maps backend status %s and code without propagating details',
  async (status, code, failure) => {
    const transport = jest.fn().mockRejectedValue({
      response: { status, data: { code, message: transaction } },
    });
    const service = new DappTransactionContextService(transport, () => binding);
    const error = await service
      .capture(binding, [transaction])
      .catch((value) => value);
    expect(error).toBeInstanceOf(DappTransactionContextServiceError);
    expect(error).toMatchObject({ failure, message: failure });
    expect(error.message).not.toContain(transaction);
  }
);

test('normalizes malformed backend context before review', async () => {
  (reconcileTransactionContext as jest.Mock).mockImplementation(() => {
    throw new Error(transaction);
  });
  const service = new DappTransactionContextService(
    jest.fn().mockResolvedValue({}),
    () => binding
  );
  await expect(service.capture(binding, [transaction])).rejects.toMatchObject({
    failure: 'internal_error',
    message: 'internal_error',
  });
});
