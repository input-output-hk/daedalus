import type { Api } from '../api';
import type { StoresMap } from '../stores';
import { Cip30WalletService } from './Cip30WalletService';

jest.mock('../ipc/cip30Wallet', () => ({
  bindCip30WalletRenderer: jest.fn(() => jest.fn()),
}));

const network = {
  networkId: 0 as const,
  networkMagic: 42,
  genesisHash: '11'.repeat(32),
};
const request = (operation: 'capabilities' | 'context' | 'addresses') => ({
  operation,
  walletId: 'wallet',
  network,
  sourceRevision: '22'.repeat(20),
});

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
  const stakeAddresses = { wallet: 'stake-address' };
  const api = ({
    ada: {
      getDappCapabilities,
      getDappTransactionContext,
      getAddresses,
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
  } as unknown) as StoresMap;
  return {
    service: new Cip30WalletService(api, stores),
    getDappCapabilities,
    getDappTransactionContext,
    getAddresses,
    setWallet: (value: Record<string, unknown> | null) => {
      wallet = value;
    },
    setReady: (value: boolean) => {
      connected = value;
      synced = value;
    },
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
});
