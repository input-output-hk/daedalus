import type { Api } from '../api';
import type { StoresMap } from '../stores';
import { Cip30WalletService } from './Cip30WalletService';

jest.mock('../ipc/cip30Wallet', () => ({
  bindCip30WalletRenderer: jest.fn(() => jest.fn()),
}));
jest.mock('../api/transactions/dappBackend', () => ({
  validateDappTransactionContext: jest.fn((value) => value),
}));

const network = {
  networkId: 0 as const,
  networkMagic: 42,
  genesisHash: '11'.repeat(32),
};
const request = (
  operation: 'capabilities' | 'context' | 'addresses' | 'cip95-key-state'
) => ({
  operation,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
});
const signDataRequest = {
  operation: 'sign-data' as const,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
  address: `60${'11'.repeat(28)}`,
  payload: '00',
  passphrase: 'secret',
};
const transactionContextRequest = {
  operation: 'transaction-context' as const,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
  transactions: ['84a0a0f5f6'],
};
const signTransactionsRequest = {
  operation: 'sign-transactions' as const,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
  context: { revision: 1 },
  transactions: [{ cbor: '84a0a0f5f6', partialSign: true }],
  passphrase: 'secret',
};
const submitTransactionRequest = {
  operation: 'submit-transaction' as const,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
  transaction: '84a0a0f5f6',
};

const create = () => {
  let wallet: Record<string, unknown> | null = {
    id: 'wallet',
    name: 'Wallet',
    isHardwareWallet: false,
  };
  let connected = true;
  let synced = true;
  const getDappCapabilities = jest.fn(async () => ({ api_version: 1 }));
  const getDappTransactionContext = jest.fn(async () => ({ context: true }));
  const getAddresses = jest.fn(async () => [
    { id: 'addr-unused-2', used: false, spendingPath: '2' },
    { id: 'addr-used', used: true, spendingPath: '0' },
    { id: 'addr-unused-1', used: false, spendingPath: '1' },
  ]);
  const signDappData = jest.fn(async () => ({
    revision: 1 as const,
    credential_kind: 'payment' as const,
    credential: '11'.repeat(28),
    cose_sign1: '00',
    cose_key: '00',
  }));
  const getDappCip95KeyState = jest.fn(async () => ({
    drep_public_key: '33'.repeat(32),
    registered_stake_public_keys: ['44'.repeat(32)],
    unregistered_stake_public_keys: ['55'.repeat(32)],
  }));
  const signDappTransactions = jest.fn(async () => ({
    revision: 1 as const,
    witnesses: [
      {
        transaction_index: 0,
        body_hash: '66'.repeat(32),
        witness_set_cbor: 'a0',
      },
    ],
  }));
  const submitDappTransaction = jest.fn(async () => ({
    revision: 1 as const,
    transaction_id: '77'.repeat(32),
    status: 'submitted' as const,
  }));
  const withWalletSendLock = jest.fn(
    async (_walletId: string, work: () => Promise<unknown>) => work()
  );
  const stakeAddresses = { wallet: 'stake-address' };
  const api = ({
    ada: {
      getDappCapabilities,
      getDappTransactionContext,
      getAddresses,
      signDappData,
      getDappCip95KeyState,
      signDappTransactions,
      submitDappTransaction,
    },
  } as unknown) as Api;
  const stores = ({
    wallets: {
      get activeDappWallet() {
        return wallet;
      },
    },
    networkStatus: {
      get isConnected() {
        return connected;
      },
      get isSynced() {
        return synced;
      },
    },
    hardwareWallets: { checkIsTrezorByWalletId: jest.fn(() => false) },
    addresses: {
      stakeAddresses,
      _getStakeAddress: jest.fn(async () => undefined),
    },
    transactions: { withWalletSendLock },
  } as unknown) as StoresMap;
  return {
    service: new Cip30WalletService(api, stores),
    getDappCapabilities,
    getDappTransactionContext,
    getAddresses,
    signDappData,
    getDappCip95KeyState,
    submitDappTransaction,
    withWalletSendLock,
    setWallet: (value: Record<string, unknown> | null) => {
      wallet = value;
    },
    setReady: (value: boolean) => {
      connected = value;
      synced = value;
    },
    signDappTransactions,
  };
};

describe('Cip30WalletService', () => {
  it('fails closed before backend access while disconnected or on account drift', async () => {
    const fixture = create();
    fixture.setReady(false);
    await expect(
      fixture.service.receive(request('capabilities'))
    ).resolves.toEqual({ status: 'rejected', reason: 'unavailable' });
    expect(fixture.getDappCapabilities).not.toHaveBeenCalled();

    fixture.setReady(true);
    fixture.setWallet(null);
    await expect(fixture.service.receive(request('context'))).resolves.toEqual({
      status: 'rejected',
      reason: 'account-change',
    });
    expect(fixture.getDappTransactionContext).not.toHaveBeenCalled();
  });

  it('returns strict capability evidence and backend context for the route wallet', async () => {
    const fixture = create();
    await expect(
      fixture.service.receive(request('capabilities'))
    ).resolves.toEqual({
      status: 'fulfilled',
      operation: 'capabilities',
      value: {
        walletId: 'wallet',
        walletName: 'Wallet',
        walletKind: 'shelley-software',
        network,
        backendApiVersion: 1,
        backendExtensions: [95, 103],
      },
    });
    expect(fixture.getDappCapabilities).toHaveBeenCalledWith({
      sourceRevision: '22'.repeat(20),
      network: {
        network_id: 0,
        network_magic: 42,
        genesis_hash: network.genesisHash,
      },
    });

    await expect(fixture.service.receive(request('context'))).resolves.toEqual({
      status: 'fulfilled',
      operation: 'context',
      value: { context: true },
    });
  });

  it('returns ordered source addresses without CIP-30 serialization', async () => {
    const fixture = create();
    await expect(
      fixture.service.receive(request('addresses'))
    ).resolves.toEqual({
      status: 'fulfilled',
      operation: 'addresses',
      value: {
        walletId: 'wallet',
        network,
        used: ['addr-used'],
        unused: ['addr-unused-1', 'addr-unused-2'],
        change: 'addr-unused-2',
        reward: ['stake-address'],
      },
    });
    expect(fixture.getAddresses).toHaveBeenCalledWith({
      walletId: 'wallet',
      isLegacy: false,
    });
  });

  it('returns authoritative CIP-95 registration classification unchanged', async () => {
    const fixture = create();
    await expect(
      fixture.service.receive(request('cip95-key-state'))
    ).resolves.toEqual({
      status: 'fulfilled',
      operation: 'cip95-key-state',
      value: {
        drep_public_key: '33'.repeat(32),
        registered_stake_public_keys: ['44'.repeat(32)],
        unregistered_stake_public_keys: ['55'.repeat(32)],
      },
    });
    expect(fixture.getDappCip95KeyState).toHaveBeenCalledWith('wallet');

    fixture.getDappCip95KeyState.mockImplementationOnce(async () => {
      fixture.setWallet(null);
      return {
        drep_public_key: '33'.repeat(32),
        registered_stake_public_keys: [],
        unregistered_stake_public_keys: [],
      };
    });
    await expect(
      fixture.service.receive(request('cip95-key-state'))
    ).resolves.toEqual({ status: 'rejected', reason: 'account-change' });
  });
  it('maps fixed CIP-95 key-state backend failures', async () => {
    const fixture = create();
    for (const [code, reason] of [
      ['dapp_account_changed', 'account-change'],
      ['dapp_context_unavailable', 'unavailable'],
      ['unexpected', 'internal'],
    ] as const) {
      fixture.getDappCip95KeyState.mockRejectedValueOnce({ code });
      await expect(
        fixture.service.receive(request('cip95-key-state'))
      ).resolves.toEqual({ status: 'rejected', reason });
    }
  });

  it('forwards exact transaction context and software witness requests', async () => {
    const fixture = create();
    await expect(
      fixture.service.receive(transactionContextRequest)
    ).resolves.toEqual({
      status: 'fulfilled',
      operation: 'transaction-context',
      value: { context: true },
    });
    expect(fixture.getDappTransactionContext).toHaveBeenLastCalledWith({
      walletId: 'wallet',
      request: {
        revision: 1,
        network: {
          network_id: 0,
          network_magic: 42,
          genesis_hash: network.genesisHash,
        },
        transactions: ['84a0a0f5f6'],
      },
    });

    await expect(
      fixture.service.receive(signTransactionsRequest)
    ).resolves.toMatchObject({
      status: 'fulfilled',
      operation: 'sign-transactions',
    });
    expect(fixture.signDappTransactions).toHaveBeenCalledWith({
      walletId: 'wallet',
      request: {
        revision: 1,
        context: { revision: 1 },
        transactions: [{ cbor: '84a0a0f5f6', partial_sign: true }],
        passphrase: 'secret',
      },
    });
  });

  it('maps transaction witness errors and rejects hardware before signing', async () => {
    const fixture = create();
    for (const [code, reason] of [
      ['dapp_tx_proof_generation', 'tx-proof-generation'],
      ['dapp_deprecated_certificate', 'deprecated-certificate'],
      ['dapp_account_changed', 'account-change'],
      ['dapp_context_unavailable', 'unavailable'],
      ['unexpected', 'internal'],
    ] as const) {
      fixture.signDappTransactions.mockRejectedValueOnce({ code });
      await expect(
        fixture.service.receive(signTransactionsRequest)
      ).resolves.toEqual({ status: 'rejected', reason });
    }
    fixture.setWallet({
      id: 'wallet',
      name: 'Hardware',
      isHardwareWallet: true,
    });
    fixture.signDappTransactions.mockClear();
    await expect(
      fixture.service.receive(signTransactionsRequest)
    ).resolves.toEqual({
      status: 'rejected',
      reason: 'tx-proof-generation',
    });
    expect(fixture.signDappTransactions).not.toHaveBeenCalled();
  });

  it('submits exact approved bytes under the wallet lock after route loss', async () => {
    const fixture = create();
    fixture.setWallet(null);
    fixture.setReady(false);
    await expect(
      fixture.service.receive(submitTransactionRequest)
    ).resolves.toEqual({
      status: 'fulfilled',
      operation: 'submit-transaction',
      value: {
        revision: 1,
        transaction_id: '77'.repeat(32),
        status: 'submitted',
      },
    });
    expect(fixture.withWalletSendLock).toHaveBeenCalledWith(
      'wallet',
      expect.any(Function)
    );
    expect(fixture.submitDappTransaction).toHaveBeenCalledWith({
      walletId: 'wallet',
      request: {
        revision: 1,
        network: {
          network_id: 0,
          network_magic: 42,
          genesis_hash: network.genesisHash,
        },
        transaction: '84a0a0f5f6',
      },
    });
  });

  it('maps fixed submission failures after authorization', async () => {
    const fixture = create();
    for (const [code, reason] of [
      ['dapp_submission_failed', 'tx-send-failure'],
      ['dapp_account_changed', 'tx-send-failure'],
      ['dapp_context_unavailable', 'tx-send-failure'],
      ['unexpected', 'internal'],
    ] as const) {
      fixture.submitDappTransaction.mockRejectedValueOnce({ code });
      await expect(
        fixture.service.receive(submitTransactionRequest)
      ).resolves.toEqual({ status: 'rejected', reason });
    }
  });

  it('binds exact sign-data bytes and preserves typed backend failures', async () => {
    const fixture = create();
    await expect(
      fixture.service.receive(signDataRequest)
    ).resolves.toMatchObject({
      status: 'fulfilled',
      operation: 'sign-data',
    });
    expect(fixture.signDappData).toHaveBeenCalledWith({
      walletId: 'wallet',
      request: {
        revision: 1,
        network: {
          network_id: 0,
          network_magic: 42,
          genesis_hash: network.genesisHash,
        },
        address: signDataRequest.address,
        payload: '00',
        passphrase: 'secret',
      },
    });

    for (const [code, reason] of [
      ['dapp_data_address_not_pk', 'address-not-pk'],
      ['dapp_data_proof_generation', 'proof-generation'],
      ['dapp_account_changed', 'account-change'],
    ] as const) {
      fixture.signDappData.mockRejectedValueOnce({ code });
      await expect(fixture.service.receive(signDataRequest)).resolves.toEqual({
        status: 'rejected',
        reason,
      });
    }
  });

  it('rejects hardware data signing before backend access', async () => {
    const fixture = create();
    fixture.setWallet({
      id: 'wallet',
      name: 'Hardware',
      isHardwareWallet: true,
    });
    await expect(fixture.service.receive(signDataRequest)).resolves.toEqual({
      status: 'rejected',
      reason: 'proof-generation',
    });
    expect(fixture.signDappData).not.toHaveBeenCalled();
  });
});
