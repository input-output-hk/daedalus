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
  getCardanoAdaAppChannel,
  getHardwareWalletTransportChannel,
  signExactHardwareTransactionChannel,
  showAddressChannel,
} from '../ipc/getHardwareWalletChannel';
import { getHardwareWalletsNetworkConfig } from '../config/hardwareWalletsConfig';
import * as transactionContext from '../../../common/cardano/transactionContext';
import * as hardwareWalletTransaction from '../utils/hardwareWalletTransaction';
import Action from '../actions/lib/Action';

import HardwareWalletsStore from './HardwareWalletsStore';
import { HwDeviceStatuses } from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';

jest.mock('../../../common/cardano/transactionContext', () => ({
  ...(jest.requireActual(
    '../../../common/cardano/transactionContext'
  ) as object),
  reconcileTransactionContext: jest.fn(),
}));
jest.mock('../utils/hardwareWalletTransaction', () => ({
  ...(jest.requireActual('../utils/hardwareWalletTransaction') as object),
  bindPaymentChange: jest.fn(),
  prepareHardwareTransaction: jest.fn(),
}));
jest.mock('@trezor/connect', () => ({}));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

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
  it('waits for address display and keeps late device outcomes out of the next operation', async () => {
    const request = jest.fn();
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
    const params = {
      address: { id: 'address', spendingPath: "1852'/1815'/0'/0/0" } as any,
      path: 'ledger-path',
      isTrezor: false,
    };
    jest.spyOn(deriveAddressChannel, 'request').mockResolvedValue('address');
    let finishDisplay: () => void;
    const display = jest
      .spyOn(showAddressChannel, 'request')
      .mockImplementationOnce(
        () =>
          new Promise<void>((resolve) => {
            finishDisplay = resolve;
          })
      );
    let settled = false;
    const verification = store.verifyAddress(params).then(() => {
      settled = true;
    });
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(settled).toBe(false);
    expect(store.isAddressChecked).toBe(false);
    finishDisplay();
    await verification;
    expect(store.isAddressChecked).toBe(true);
    expect(store.hwDeviceStatus).toBe(
      HwDeviceStatuses.VERIFYING_ADDRESS_CONFIRMATION
    );

    display.mockRejectedValueOnce(new Error('Device display failed'));
    await expect(store.verifyAddress(params)).resolves.toBeUndefined();
    expect(store.hwDeviceStatus).toBe(
      HwDeviceStatuses.VERIFYING_ADDRESS_FAILED
    );

    let rejectDisplay: (error: Error) => void;
    display.mockImplementationOnce(
      () =>
        new Promise<void>((_resolve, reject) => {
          rejectDisplay = reject;
        })
    );
    const staleVerification = store.verifyAddress(params);
    await new Promise((resolve) => setTimeout(resolve, 0));
    await store.resetInitializedAddressVerification({
      cancelDeviceAction: false,
    });
    store.hwDeviceStatus = HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED;
    rejectDisplay(new Error('Action rejected by user on device.'));
    await expect(staleVerification).resolves.toBeUndefined();
    expect(store.hwDeviceStatus).toBe(
      HwDeviceStatuses.VERIFYING_TRANSACTION_SUCCEEDED
    );
    expect(store.isAddressChecked).toBe(false);
    jest.restoreAllMocks();
  });

  it('does not report an unknown backend submission as a successful payment', async () => {
    const submit = jest.fn();
    const store = new HardwareWalletsStore(
      ({
        ada: { submitDappTransaction: submit },
        localStorage: {},
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    (store as any).stores = {
      networkStatus: { genesisBlockHash: '00'.repeat(32) },
    };
    store.txSignRequest = {
      coinSelection: coinSelection as any,
      exactPayment: { signedTransaction: '84a0a0f5f6' } as any,
    };
    const transactionId = 'ab'.repeat(32);
    submit.mockResolvedValue({
      status: 'submitted',
      transaction_id: transactionId,
    });
    await expect((store as any).submitExactPayment('wallet')).resolves.toEqual({
      status: 'submitted',
      transactionIds: [transactionId],
    });
    submit.mockResolvedValue({
      status: 'outcome_unknown',
      transaction_id: transactionId,
    });
    await expect((store as any).submitExactPayment('wallet')).resolves.toEqual({
      status: 'submission-unknown',
      transactionIds: [transactionId],
    });
    submit.mockResolvedValue({
      status: 'rejected',
      transaction_id: transactionId,
    });
    await expect((store as any).submitExactPayment('wallet')).resolves.toEqual({
      status: 'rejected',
      errorCode: 'rejected',
    });
  });

  it('defers transaction context capture until signing and binds the selected transaction', async () => {
    const constructTransaction = jest.fn().mockResolvedValue({
      transaction: '84a0a0f5f6',
      coinSelection,
    });
    const getDappTransactionContext = jest.fn();
    const acquireWalletSendLock = jest.fn();
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
        acquireWalletSendLock,
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
    expect(acquireWalletSendLock).not.toHaveBeenCalled();
  });

  it('prepares native Ledger payments without dApp certification', async () => {
    const request = jest.fn().mockResolvedValue({});
    const store = new HardwareWalletsStore(
      ({
        ada: { getDappTransactionContext: request },
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          unsetHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
          overrideHardwareWalletDevices: request,
          unsetHardwareWalletDevice: request,
          unsetHardwareWalletDevicesAll: request,
        },
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    (store as any).stores = {
      networkStatus: { genesisBlockHash: '00'.repeat(32) },
    };
    store.hardwareWalletsLocalDataRequest.result = {
      wallet: {
        id: 'wallet',
        device: { deviceType: 'ledger' },
        extendedPublicKey: {
          publicKeyHex: 'ab'.repeat(32),
          chainCodeHex: 'cd'.repeat(32),
        },
      },
    } as any;
    store.txSignRequest = {
      coinSelection: coinSelection as any,
      exactPayment: {
        unsignedTransaction: '84a0a0f5f6',
        isCollateralPreparation: false,
      },
    };
    jest
      .spyOn(transactionContext, 'reconcileTransactionContext')
      .mockReturnValue({} as any);
    jest
      .spyOn(hardwareWalletTransaction, 'prepareHardwareTransaction')
      .mockImplementation((_snapshot, _index, _partial, capability) =>
        capability.productEnabled
          ? ({
              status: 'ready',
              exact: { capability },
            } as any)
          : ({ status: 'unsupported' } as any)
      );
    jest
      .spyOn(hardwareWalletTransaction, 'bindPaymentChange')
      .mockImplementation(async (exact) => exact);

    await expect((store as any).prepareExactPayment('wallet')).resolves.toEqual(
      expect.objectContaining({
        preparation: expect.objectContaining({
          status: 'ready',
          exact: expect.objectContaining({
            capability: expect.objectContaining({
              rowId: 'ledger-native',
              productEnabled: true,
            }),
          }),
        }),
      })
    );
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
        path: 'previous-ledger-path',
        device: {
          deviceType: 'ledger',
          deviceModel: 'europa',
          path: 'stale-ledger-path',
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

  it('refreshes the paired Ledger connection before native signing and refuses a locked app', async () => {
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
          path: 'stale-ledger-path',
        },
      },
    } as any;
    const connected = {
      deviceType: 'ledger',
      deviceModel: 'europa',
      path: 'current-ledger-path',
      disconnected: false,
    };
    store.connectedHardwareWalletsDevices.set(connected.path, connected as any);
    store.stores = {
      wallets: { refreshWalletsData: jest.fn() },
    } as any;
    const stored = store.hardwareWalletsLocalDataRequest.result;
    request.mockImplementation(async (walletId, data) => {
      if (walletId) Object.assign(stored[walletId], data);
      return stored;
    });
    const app = jest
      .spyOn(getCardanoAdaAppChannel, 'request')
      .mockResolvedValue({
        major: '7',
        minor: '3',
        patch: '1',
      } as any);
    const signTransaction = jest
      .spyOn(signExactHardwareTransactionChannel, 'request')
      .mockResolvedValue({
        witnessSetCbor: 'a0',
        signedTransactionCbor: '84a0a0f5f6',
      });
    const exportKey = jest.spyOn(store, '_requestExtendedPublicKey');
    const preparation = {
      status: 'ready',
      exact: {
        capability: (store as any).getNativeTransactionCapability('wallet'),
      },
    };
    await (store as any).signExactTransaction('wallet', preparation);
    expect(signTransaction).toHaveBeenCalledWith(
      expect.objectContaining({
        walletId: 'wallet',
        vendor: 'ledger',
        ledgerPath: connected.path,
      })
    );

    await store._changeHardwareWalletConnectionStatus({
      disconnected: true,
      deviceType: 'ledger',
      path: connected.path,
    } as any);
    store.connectedHardwareWalletsDevices.set(connected.path, connected as any);
    app.mockRejectedValueOnce(new Error('Device is locked'));
    await expect(
      (store as any).signExactTransaction('wallet', preparation)
    ).rejects.toThrow('Device is locked');
    expect(signTransaction).toHaveBeenCalledTimes(1);
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

  it('settles the payment action and clears pending state after an unknown outcome', async () => {
    const request = jest.fn();
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
    store.stores = {
      wallets: { active: { id: 'wallet-id' } },
      walletApproval: {
        nativeTransactions: {
          run: jest.fn().mockResolvedValue({
            status: 'submission-unknown',
            transactionIds: ['77'.repeat(32)],
          }),
        },
      },
    } as any;
    store.txSignRequest = {
      coinSelection: coinSelection as any,
      exactPayment: { unsignedTransaction: '84a0a0f5f6' } as any,
    };
    store.setTransactionPendingState(true);
    const send = new Action();
    send.listen(store._sendMoney);
    await send.trigger();
    expect(store.isTransactionPending).toBe(false);
  });
});
