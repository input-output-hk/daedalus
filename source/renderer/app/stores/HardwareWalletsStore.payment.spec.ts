jest.mock('@trezor/connect', () => ({}));
import {
  _seedToKeypairV2,
  derivePrivate,
  derivePublic,
} from 'cardano-crypto.js';
import { blake2b } from 'blakejs';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import {
  deriveXpubChannel,
  deriveAddressChannel,
} from '../ipc/getHardwareWalletChannel';
import { getHardwareWalletsNetworkConfig } from '../config/hardwareWalletsConfig';
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

import HardwareWalletsStore from './HardwareWalletsStore';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';
import {
  getCardanoAdaAppChannel,
  getHardwareWalletTransportChannel,
} from '../ipc/getHardwareWalletChannel';

const coinSelection = {
  inputs: [],
  outputs: [],
  certificates: [],
  deposits: null,
  depositsReclaimed: null,
  withdrawals: [],
  fee: null,
  metadata: null,
};

describe('HardwareWalletsStore payment construction', () => {
  it('defers transaction context capture until signing and binds the selected transaction', async () => {
    const constructTransaction = jest.fn().mockResolvedValue({
      transaction: '84a0a0f5f6',
      coinSelection,
    });
    const getDappTransactionContext = jest.fn();
    const release = jest.fn();
    const request = jest.fn();
    const store = new HardwareWalletsStore(
      ({
        ada: {
          selectCoins: request,
          createExternalTransaction: request,
          getPublicKey: request,
          constructAddress: request,
          constructTransaction,
          getDappTransactionContext,
        },
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          unsetHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
          overrideHardwareWalletDevices: request,
          unsetHardwareWalletDevice: request,
          unsetHardwareWalletDevicesAll: request,
          unsetHardwareWalletLocalDataAll: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    (store as any).stores = {
      wallets: { getWalletById: () => ({ id: 'wallet' }) },
      transactions: {
        acquireWalletSendLock: jest.fn().mockResolvedValue({ release }),
      },
    };

    const selected = await store.selectCoins({
      walletId: 'wallet',
      address: 'addr1receiver',
      amount: 5_000_000,
    });

    expect(getDappTransactionContext).not.toHaveBeenCalled();
    expect(selected.unsignedTransaction).toBe('84a0a0f5f6');

    store.updateTxSignRequest(selected, true);

    expect(store.txSignRequest.exactPayment).toEqual({
      unsignedTransaction: '84a0a0f5f6',
      isCollateralPreparation: true,
    });
    (store as any).releaseWalletSendLease();
    expect(release).toHaveBeenCalledTimes(1);
  });

  it('refreshes a Ledger path before every transaction', async () => {
    const request = jest.fn();
    const store = new HardwareWalletsStore(
      ({
        ada: {
          selectCoins: request,
          createExternalTransaction: request,
          getPublicKey: request,
          constructAddress: request,
          constructTransaction: request,
          getDappTransactionContext: request,
        },
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          unsetHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
          overrideHardwareWalletDevices: request,
          unsetHardwareWalletDevice: request,
          unsetHardwareWalletDevicesAll: request,
          unsetHardwareWalletLocalDataAll: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    store.hardwareWalletsLocalDataRequest.result = {
      'wallet-id': {
        id: 'wallet-id',
        disconnected: false,
        device: { deviceType: 'ledger', path: 'stale-ledger-path' },
      },
    } as any;
    const waitForLedgerTransportDevice = jest
      .spyOn(store, 'waitForLedgerTransportDevice')
      .mockResolvedValue({ path: 'unexpected-path' } as any);
    const openTransport = jest
      .spyOn(getHardwareWalletTransportChannel, 'request')
      .mockResolvedValue({
        deviceType: 'ledger',
        path: 'current-ledger-path',
      } as any);
    const useCardanoAppInterval = jest
      .spyOn(store, 'useCardanoAppInterval')
      .mockImplementation(jest.fn());
    jest
      .spyOn(store, 'stopCardanoAdaAppFetchPoller')
      .mockImplementation(jest.fn());

    await store.initiateTransaction({ walletId: 'wallet-id' });

    expect(waitForLedgerTransportDevice).not.toHaveBeenCalled();
    expect(openTransport).toHaveBeenCalledWith({
      devicePath: 'stale-ledger-path',
      isTrezor: false,
    });
    expect(store.activeDevicePath).toBe('current-ledger-path');
    expect(useCardanoAppInterval).toHaveBeenCalledWith(
      'current-ledger-path',
      'wallet-id'
    );
  });

  it('refreshes recognized Ledger data before continuing transaction signing', async () => {
    let startRefresh = () => {};
    let finishRefresh: (value: unknown) => void = () => {};
    const refreshStarted = new Promise<void>((resolve) => {
      startRefresh = resolve;
    });
    const getHardwareWalletsLocalData = jest.fn(
      () =>
        new Promise((resolve) => {
          finishRefresh = resolve;
          startRefresh();
        })
    );
    const request = jest.fn().mockResolvedValue(null);
    const store = new HardwareWalletsStore(
      ({
        ada: {
          selectCoins: request,
          createExternalTransaction: request,
          getPublicKey: request,
          constructAddress: request,
          constructTransaction: request,
          getDappTransactionContext: request,
        },
        localStorage: {
          getHardwareWalletsLocalData,
          setHardwareWalletLocalData: request,
          unsetHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
          overrideHardwareWalletDevices: request,
          unsetHardwareWalletDevice: request,
          unsetHardwareWalletDevicesAll: request,
          unsetHardwareWalletLocalDataAll: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    (store as any).stores = {
      wallets: { refreshWalletsData: jest.fn() },
    };
    store.transportDevice = ({
      deviceType: 'ledger',
      deviceName: 'Ledger Flex',
      deviceModel: 'europa',
      path: 'current-ledger-path',
    } as unknown) as any;
    store.isTransactionInitiated = true;
    jest
      .spyOn(store, '_deletePendingDeviceWithGivenPath')
      .mockResolvedValue(undefined);
    const proceed = jest
      .spyOn(store, '_proceedWithTransactionAfterConnectingDevice')
      .mockImplementation(jest.fn());

    const handling = store._storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting(
      {
        associatedWallet: { id: 'wallet-id' } as any,
        expectedWalletId: 'wallet-id',
        extendedPublicKey: {
          publicKeyHex: '00'.repeat(32),
          chainCodeHex: '00'.repeat(32),
        } as any,
        path: 'current-ledger-path',
      }
    );
    await refreshStarted;

    expect(proceed).not.toHaveBeenCalled();
    expect(request).toHaveBeenCalledWith('wallet-id', {
      disconnected: false,
      device: {
        deviceType: 'ledger',
        deviceName: 'Ledger Flex',
        deviceModel: 'europa',
        path: 'current-ledger-path',
      },
    });

    finishRefresh({
      'wallet-id': {
        id: 'wallet-id',
        disconnected: false,
        device: {
          deviceType: 'ledger',
          deviceName: 'Ledger Flex',
          deviceModel: 'europa',
          path: 'current-ledger-path',
        },
      },
    });
    await handling;

    expect(proceed).toHaveBeenCalledWith({
      isTrezor: false,
      deviceId: undefined,
      devicePath: 'current-ledger-path',
      walletId: 'wallet-id',
    });
  });

  it('signs exact Ledger payments without exporting the account key', async () => {
    const request = jest.fn().mockResolvedValue(null);
    const store = new HardwareWalletsStore(
      ({
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    store.hardwareWalletsLocalDataRequest.result = {
      'wallet-id': {
        id: 'wallet-id',
        disconnected: true,
        extendedPublicKey: {
          publicKeyHex: 'ab'.repeat(32),
          chainCodeHex: 'cd'.repeat(32),
        },
        device: {
          deviceType: 'ledger',
          deviceName: 'Ledger Flex',
          deviceModel: 'europa',
          path: 'stale-ledger-path',
        },
      },
    } as any;
    store.isTransactionInitiated = true;
    store.txSignRequest = {
      coinSelection: coinSelection as any,
      exactPayment: {
        unsignedTransaction: '84a0a0f5f6',
        isCollateralPreparation: false,
      },
    };
    jest.spyOn(getCardanoAdaAppChannel, 'request').mockResolvedValue({
      major: '7',
      minor: '3',
      patch: '1',
    } as any);
    jest
      .spyOn(store, 'stopCardanoAdaAppFetchPoller')
      .mockImplementation(jest.fn());
    const identify = jest.spyOn(store, '_identifyAndHandleAssociatedWallet');
    const setConnection = jest
      .spyOn(store, '_setHardwareWalletLocalData')
      .mockResolvedValue(undefined);
    const proceed = jest
      .spyOn(store, '_proceedWithTransactionAfterConnectingDevice')
      .mockImplementation(jest.fn());

    await store.getCardanoAdaApp({
      path: 'current-ledger-path',
      walletId: 'wallet-id',
    });

    expect(identify).not.toHaveBeenCalled();
    expect(setConnection).toHaveBeenCalledWith({
      walletId: 'wallet-id',
      data: {
        disconnected: false,
        device: {
          deviceType: 'ledger',
          deviceName: 'Ledger Flex',
          deviceModel: 'europa',
          path: 'current-ledger-path',
        },
      },
    });
    expect(proceed).toHaveBeenCalledWith({
      isTrezor: false,
      devicePath: 'current-ledger-path',
      walletId: 'wallet-id',
      deviceId: undefined,
    });
  });

  it('discovers a connected Ledger after restart without exporting keys or retaining failed version evidence', async () => {
    const request = jest.fn().mockResolvedValue(null);
    const store = new HardwareWalletsStore(
      ({
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    store.hardwareWalletsLocalDataRequest.result = {
      wallet: {
        id: 'wallet',
        disconnected: true,
        device: {
          deviceType: 'ledger',
          deviceModel: 'europa',
          path: 'ledger-path',
        },
      },
    } as any;
    store.connectedHardwareWalletsDevices.set('ledger-path', {
      deviceType: 'ledger',
      deviceModel: 'europa',
      path: 'ledger-path',
      disconnected: false,
    } as any);
    store.stores = {
      wallets: { refreshWalletsData: jest.fn() },
    } as any;
    const stored = store.hardwareWalletsLocalDataRequest.result;
    request.mockImplementation(async (walletId, data) => {
      if (walletId) Object.assign(stored[walletId], data);
      return stored;
    });
    const exportKey = jest.spyOn(store, '_requestExtendedPublicKey');
    const app = jest
      .spyOn(getCardanoAdaAppChannel, 'request')
      .mockResolvedValue({
        major: '7',
        minor: '3',
        patch: '1',
      } as any);
    expect(store.getDappConnectorCapability('wallet')).toBeUndefined();
    await expect(
      store.refreshDappConnectorCapability('wallet')
    ).resolves.toMatchObject({
      model: 'europa',
      appVersion: '7.3.1',
    });
    app.mockRejectedValueOnce(new Error('Cardano app unavailable'));
    const capability = store.getDappConnectorCapability('wallet');
    await expect(
      Promise.all([
        store.refreshDappConnectorCapability('wallet'),
        store.refreshDappConnectorCapability('wallet'),
        store.refreshDappConnectorCapability('wallet'),
      ])
    ).resolves.toEqual([capability, capability, capability]);
    await store._changeHardwareWalletConnectionStatus({
      disconnected: true,
      deviceType: 'ledger',
      path: 'ledger-path',
    } as any);
    store.connectedHardwareWalletsDevices.set('ledger-path', {
      deviceType: 'ledger',
      deviceModel: 'europa',
      path: 'ledger-path',
      disconnected: false,
    } as any);
    stored.wallet.disconnected = false;
    await expect(
      store.refreshDappConnectorCapability('wallet')
    ).rejects.toThrow('Cardano app unavailable');
    expect(store.getDappConnectorCapability('wallet')).toBeUndefined();
    store.connectedHardwareWalletsDevices.clear();
    await expect(
      store.refreshDappConnectorCapability('wallet')
    ).resolves.toBeUndefined();
    expect(exportKey).not.toHaveBeenCalled();
  });

  it('resumes paired transactions and address display without exporting, but keeps pairing explicit', async () => {
    const account = await _seedToKeypairV2(
      Buffer.alloc(16, 9),
      Buffer.alloc(0)
    );
    const accountXpub = account.subarray(64);
    const deriveHash = (role: number) =>
      Buffer.from(
        blake2b(
          derivePrivate(derivePrivate(account, role, 2), 0, 2).subarray(64, 96),
          undefined,
          28
        )
      );
    const networkId = getHardwareWalletsNetworkConfig(
      global.environment.network
    ).networkId;
    const address = {
      id: utils.bech32_encodeAddress(
        Buffer.concat([Buffer.from([networkId]), deriveHash(0), deriveHash(2)])
      ),
      spendingPath: "1852'/1815'/0'/0/0",
    } as any;
    const request = jest.fn().mockResolvedValue({});
    const store = new HardwareWalletsStore(
      ({
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      { sendEvent: jest.fn() } as any
    );
    const key = {
      publicKeyHex: accountXpub.subarray(0, 32).toString('hex'),
      chainCodeHex: accountXpub.subarray(32).toString('hex'),
    };
    const exportKey = jest
      .spyOn(store, '_requestExtendedPublicKey')
      .mockResolvedValue(key as any);
    jest
      .spyOn(store, '_findAssociatedWalletByExtendedPublicKey')
      .mockResolvedValue({ id: 'wallet-id' } as any);
    const pairedImport = jest
      .spyOn(
        store,
        '_storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting'
      )
      .mockResolvedValue(undefined);
    jest
      .spyOn(store, '_setHardwareWalletLocalData')
      .mockResolvedValue(undefined);
    const transact = jest
      .spyOn(store, '_proceedWithTransactionAfterConnectingDevice')
      .mockImplementation(jest.fn());
    const displayAddress = jest
      .spyOn(store, 'showAddress')
      .mockResolvedValue(undefined);
    const deriveAddress = jest
      .spyOn(deriveAddressChannel, 'request')
      .mockResolvedValue(address.id);
    jest
      .spyOn(deriveXpubChannel, 'request')
      .mockImplementation(
        async ({ parentXpubHex, lastIndex, derivationScheme }) =>
          derivePublic(
            Buffer.from(parentXpubHex, 'hex'),
            lastIndex,
            derivationScheme
          ).toString('hex')
      );
    for (const vendor of ['ledger', 'trezor'] as const) {
      store.transportDevice = {
        deviceType: vendor,
        path: 'device-path',
      } as any;
      store.hardwareWalletsLocalDataRequest.result = {
        'wallet-id': {
          id: 'wallet-id',
          device: { deviceType: vendor },
          extendedPublicKey: key,
        },
      } as any;
      // Covers reconnect/identification entry and non-exact transactions.
      store.txSignRequest = { coinSelection: coinSelection as any };
      store.isTransactionInitiated = true;
      await store._identifyAndHandleAssociatedWallet({
        expectedWalletId: 'wallet-id',
        path: 'device-path',
      });
      expect(transact).toHaveBeenLastCalledWith(
        expect.objectContaining({
          walletId: 'wallet-id',
          isTrezor: vendor === 'trezor',
        })
      );
      store.isAddressVerificationInitiated = true;
      await store._identifyAndHandleAssociatedWallet({
        expectedWalletId: 'wallet-id',
        path: 'device-path',
        address,
      });
      expect(deriveAddress).toHaveBeenLastCalledWith(
        expect.objectContaining({
          isTrezor: vendor === 'trezor',
          spendingPathStr: address.spendingPath,
        })
      );
      expect(exportKey).not.toHaveBeenCalled();
      await store._identifyAndHandleAssociatedWallet({
        expectedWalletId: 'wallet-id',
        path: 'device-path',
        address,
      });
      expect(deriveAddress).toHaveBeenCalledTimes(vendor === 'ledger' ? 1 : 2);
      const displayed = deriveAddress.mock.calls.length;
      await expect(
        store._identifyAndHandleAssociatedWallet({
          expectedWalletId: 'wallet-id',
          path: 'device-path',
          address: { ...address, spendingPath: "1852'/1815'/0'/0/1" },
        })
      ).rejects.toThrow('does not belong');
      expect(deriveAddress).toHaveBeenCalledTimes(displayed);
      store.hardwareWalletsLocalDataRequest.result = {} as any;
      await expect(
        store._identifyAndHandleAssociatedWallet({
          expectedWalletId: 'wallet-id',
          path: 'device-path',
        })
      ).rejects.toThrow('pairing');
      expect(exportKey).not.toHaveBeenCalled();
    }
    expect(displayAddress).toHaveBeenCalledTimes(1);
    await store._identifyAndHandleAssociatedWallet({ path: 'device-path' });
    expect(exportKey).toHaveBeenCalledTimes(1);
    expect(pairedImport).toHaveBeenCalled();
  });
});
