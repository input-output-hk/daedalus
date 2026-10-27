import {
  _seedToKeypairV2,
  derivePrivate,
  derivePublic,
} from 'cardano-crypto.js';
import { blake2b } from 'blakejs';
import BigNumber from 'bignumber.js';
import { runInAction } from 'mobx';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import {
  deriveXpubChannel,
  deriveAddressChannel,
  getCardanoAdaAppChannel,
  getHardwareWalletTransportChannel,
  signExactHardwareTransactionChannel,
  signTransactionLedgerChannel,
  showAddressChannel,
} from '../ipc/getHardwareWalletChannel';
import { getHardwareWalletsNetworkConfig } from '../config/hardwareWalletsConfig';
import * as transactionContext from '../../../common/cardano/transactionContext';
import * as hardwareWalletTransaction from '../utils/hardwareWalletTransaction';
import Action from '../actions/lib/Action';

import HardwareWalletsStore from './HardwareWalletsStore';
import { HwDeviceStatuses, WalletUnits } from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';
import type { StoresMap } from './index';
import type {
  CoinSelectionsPaymentRequestType,
  CoinSelectionsResponse,
  ConstructTransactionData,
} from '../api/transactions/types';
import type { NativeTransactionOperation } from '../services/NativeTransactionService';
import type { DappNetwork } from '../api/transactions/dappBackend';
import type {
  HardwareTransactionCapability,
  HardwareTransactionPreparation,
} from '../../../common/types/hardware-wallets.types';
import type { TransactionContextSnapshot } from '../../../common/cardano/transactionContext';

const constructionData: ConstructTransactionData['data'] = {
  encoding: 'base16',
  payments: [
    {
      address: 'addr1receiver',
      amount: { quantity: 5_000_000, unit: WalletUnits.LOVELACE },
    },
  ],
  withdrawal: 'self',
  metadata: null,
};
const network: DappNetwork = {
  network_id: 0,
  network_magic: 1,
  genesis_hash: '00'.repeat(32),
};

type ExactPaymentHelpers = {
  prepareExactPayment: (
    walletId: string,
    data: ConstructTransactionData['data']
  ) => Promise<unknown>;
  submitExactPayment: (
    walletId: string,
    signed: string,
    network: DappNetwork,
    collateral: boolean
  ) => Promise<unknown>;
  signExactPayment: (walletId: string) => Promise<void>;
};

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
  const originalCip30Enabled = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = true;
  });
  afterEach(() => {
    global.cip30Enabled = originalCip30Enabled;
  });
  it('reviews and signs fresh locked selection Y, never preview X or a mutated draft', async () => {
    const preview = {
      ...coinSelection,
      deposits: new BigNumber(0),
      depositsReclaimed: new BigNumber(0),
    };
    const authoritative = { ...preview, outputs: [] };
    const construct = jest
      .fn()
      .mockResolvedValueOnce({
        transaction: 'preview-X',
        coinSelection: preview,
      })
      .mockResolvedValueOnce({
        transaction: 'authoritative-Y',
        coinSelection: authoritative,
      });
    const submit = jest.fn().mockResolvedValue({
      status: 'submitted',
      transaction_id: 'ab'.repeat(32),
    });
    const store = new HardwareWalletsStore(
      {
        ada: {
          constructTransaction: construct,
          getDappTransactionContext: jest.fn(),
          submitDappTransaction: submit,
        },
        localStorage: {},
      } as unknown as Api,
      {} as ActionsMap,
      { sendEvent: jest.fn() } as unknown as AnalyticsTracker
    );
    const ready = {
      status: 'ready',
      exact: {
        capability: {
          vendor: 'ledger',
          productEnabled: true,
          physicalCertified: true,
        },
      },
    } as unknown as Extract<
      HardwareTransactionPreparation,
      { status: 'ready' }
    >;
    jest
      .spyOn(transactionContext, 'reconcileTransactionContext')
      .mockReturnValue({} as TransactionContextSnapshot);
    jest
      .spyOn(hardwareWalletTransaction, 'prepareHardwareTransaction')
      .mockReturnValue(ready);
    const bindChange = jest
      .spyOn(hardwareWalletTransaction, 'bindPaymentChange')
      .mockImplementation(async (exact) => exact);
    const signer = jest
      .spyOn(
        store as unknown as {
          signExactTransaction: (
            walletId: string,
            preparation: HardwareTransactionPreparation
          ) => Promise<{ signedTransactionCbor: string }>;
        },
        'signExactTransaction'
      )
      .mockImplementation(async (_walletId, preparation) => {
        expect(preparation).toEqual(ready);
        store.txSignRequest = {
          coinSelection: preview as unknown as CoinSelectionsResponse,
          exactPayment: {
            unsignedTransaction: 'mutated',
            constructionData,
            isCollateralPreparation: true,
          },
        };
        store.stores.networkStatus.genesisBlockHash = 'ff'.repeat(32);
        return { signedTransactionCbor: 'signed-Y' };
      });
    let reviewed: NativeTransactionOperation | undefined;
    store.stores = {
      wallets: {
        active: { id: 'wallet' },
        getWalletById: () => ({ id: 'wallet' }),
        refreshWalletsData: jest.fn(),
      },
      networkStatus: { genesisBlockHash: network.genesis_hash },
      walletApproval: {
        nativeTransactions: {
          run: async (operation: NativeTransactionOperation) => {
            reviewed = operation;
            const prepared = await operation.prepare();
            expect(prepared.items[0]).toEqual(
              expect.objectContaining({ cbor: 'authoritative-Y' })
            );
            return operation.execute(
              prepared,
              undefined,
              operation.ownerSignal,
              async () => undefined
            );
          },
        },
      },
    } as unknown as StoresMap;
    jest
      .spyOn(
        store as unknown as {
          getNativeTransactionCapability: () => HardwareTransactionCapability;
        },
        'getNativeTransactionCapability'
      )
      .mockReturnValue(ready.exact.capability);
    const metadata = { 61284: { map: [] } } as unknown as NonNullable<
      ConstructTransactionData['data']['metadata']
    >;
    store.updateTxSignRequest(
      await store.selectCoins({
        walletId: 'wallet',
        address: 'addr1receiver',
        amount: 5_000_000,
        metadata,
        withdrawal: 'self',
      })
    );
    await store._sendMoney();
    clearInterval(store.checkTransactionTimeInterval);
    expect(reviewed).toBeDefined();
    expect(construct).toHaveBeenCalledTimes(2);
    expect(construct.mock.calls[1][0].data).toEqual(
      construct.mock.calls[0][0].data
    );
    expect(construct.mock.calls[1][0].data.metadata).toEqual(metadata);
    expect(signer).toHaveBeenCalledTimes(1);
    expect(bindChange).toHaveBeenCalledWith(
      ready.exact,
      authoritative.outputs,
      expect.any(String)
    );
    expect(bindChange.mock.calls[0][1]).toBe(authoritative.outputs);
    expect(bindChange.mock.calls[0][1]).not.toBe(preview.outputs);
    expect(submit).toHaveBeenCalledWith({
      walletId: 'wallet',
      request: {
        revision: 1,
        network: expect.objectContaining({
          genesis_hash: network.genesis_hash,
        }),
        transaction: 'signed-Y',
      },
    });
    jest.restoreAllMocks();
  });

  it('does not construct or sign after reset while an initiated exact payment waits for its lease', async () => {
    let acquire!: (lease: { release: () => void }) => void;
    const release = jest.fn();
    const construct = jest.fn();
    const store = new HardwareWalletsStore(
      {
        ada: { constructTransaction: construct },
        localStorage: {},
      } as unknown as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    store.stores = {
      transactions: {
        acquireWalletSendLock: () =>
          new Promise<{ release: () => void }>((resolve) => {
            acquire = resolve;
          }),
      },
    } as unknown as StoresMap;
    store.txSignRequest = {
      coinSelection: coinSelection as unknown as CoinSelectionsResponse,
      exactPayment: {
        unsignedTransaction: 'preview-X',
        constructionData,
        isCollateralPreparation: false,
      },
    };
    const signing = (store as unknown as ExactPaymentHelpers).signExactPayment(
      'wallet'
    );
    store._resetTxSignRequestData();
    acquire({ release });
    await signing;
    expect(construct).not.toHaveBeenCalled();
    expect(release).toHaveBeenCalledTimes(1);
  });

  it('waits for address display and keeps late device outcomes out of the next operation', async () => {
    const request = jest.fn();
    const store = new HardwareWalletsStore(
      {
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown as Api,
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
      {
        ada: { submitDappTransaction: submit },
        localStorage: {},
      } as unknown as Api,
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
    await expect(
      (store as unknown as ExactPaymentHelpers).submitExactPayment(
        'wallet',
        'signed',
        network,
        false
      )
    ).resolves.toEqual({
      status: 'submitted',
      transactionIds: [transactionId],
    });
    expect(submit).toHaveBeenLastCalledWith({
      walletId: 'wallet',
      request: { revision: 1, network, transaction: 'signed' },
    });
    submit.mockResolvedValue({
      status: 'outcome_unknown',
      transaction_id: transactionId,
    });
    await expect(
      (store as unknown as ExactPaymentHelpers).submitExactPayment(
        'wallet',
        'signed',
        network,
        false
      )
    ).resolves.toEqual({
      status: 'submission-unknown',
      transactionIds: [transactionId],
    });
    submit.mockResolvedValue({
      status: 'rejected',
      transaction_id: transactionId,
    });
    await expect(
      (store as unknown as ExactPaymentHelpers).submitExactPayment(
        'wallet',
        'signed',
        network,
        false
      )
    ).resolves.toEqual({
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
      {
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
      } as unknown as Api,
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
      withdrawal: 'self',
    });

    expect(getDappTransactionContext).not.toHaveBeenCalled();
    expect(selected.unsignedTransaction).toBe('84a0a0f5f6');

    store.updateTxSignRequest(selected, true);

    expect(store.txSignRequest.exactPayment).toEqual({
      unsignedTransaction: '84a0a0f5f6',
      isCollateralPreparation: true,
      constructionData,
    });
    expect(acquireWalletSendLock).not.toHaveBeenCalled();
  });

  it('constructs a payment without a withdrawal key when the request carries none', async () => {
    const constructTransaction = jest.fn().mockResolvedValue({
      transaction: '84a0a0f5f6',
      coinSelection,
    });
    const store = new HardwareWalletsStore(
      { ada: { constructTransaction }, localStorage: {} } as unknown as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    (store as any).stores = {
      wallets: { getWalletById: () => ({ id: 'wallet' }) },
    };

    const selected = await store.selectCoins({
      walletId: 'wallet',
      address: 'addr1receiver',
      amount: 5_000_000,
      withdrawal: null,
    });

    const { withdrawal, ...withoutWithdrawal } = constructionData;
    expect(withdrawal).toBe('self');
    expect(constructTransaction).toHaveBeenCalledWith({
      walletId: 'wallet',
      data: withoutWithdrawal,
    });
    expect(selected.constructionData).toEqual(withoutWithdrawal);
    expect('withdrawal' in selected.constructionData).toBe(false);
  });

  it('prepares native Ledger payments without dApp certification', async () => {
    const request = jest.fn().mockResolvedValue({});
    const store = new HardwareWalletsStore(
      {
        ada: {
          getDappTransactionContext: request,
          constructTransaction: jest.fn().mockResolvedValue({
            transaction: 'authoritative',
            coinSelection,
          }),
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
        },
      } as unknown as Api,
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
        constructionData,
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

    await expect(
      (store as unknown as ExactPaymentHelpers).prepareExactPayment(
        'wallet',
        constructionData
      )
    ).resolves.toEqual(
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
      {
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
      } as unknown as Api,
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
      {
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
      } as unknown as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    (store as any).stores = {
      wallets: { refreshWalletsData: jest.fn() },
    };
    store.transportDevice = {
      deviceType: 'ledger',
      deviceName: 'Ledger Flex',
      deviceModel: 'europa',
      path: 'current-ledger-path',
    } as unknown as any;
    store.isTransactionInitiated = true;
    jest
      .spyOn(store, '_deletePendingDeviceWithGivenPath')
      .mockResolvedValue(undefined);
    const proceed = jest
      .spyOn(store, '_proceedWithTransactionAfterConnectingDevice')
      .mockImplementation(jest.fn());

    const handling =
      store._storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting(
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
      {
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown as Api,
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
        constructionData,
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

  it('discovers a Ledger after restart while accepting newer compatible app versions', async () => {
    const request = jest.fn().mockResolvedValue(null);
    const store = new HardwareWalletsStore(
      {
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown as Api,
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
        minor: '4',
        patch: '0',
      } as any);
    expect(store.getDappConnectorCapability('wallet')).toBeUndefined();
    await expect(
      store.refreshDappConnectorCapability('wallet')
    ).resolves.toMatchObject({
      model: 'europa',
      appVersion: '7.4.0',
      certifiedExtensions: [95, 104],
      physicalCertified: true,
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
      {
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown as Api,
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
      {
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown as Api,
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
    store.isWalletPairingInitiated = true;
    await store._identifyAndHandleAssociatedWallet({ path: 'device-path' });
    expect(exportKey).toHaveBeenCalledTimes(1);
    expect(pairedImport).toHaveBeenCalled();
  });

  it('settles the payment action and clears pending state after an unknown outcome', async () => {
    const request = jest.fn();
    const store = new HardwareWalletsStore(
      {
        ada: {},
        localStorage: {
          getHardwareWalletsLocalData: request,
          setHardwareWalletLocalData: request,
          getHardwareWalletDevices: request,
          setHardwareWalletDevice: request,
        },
      } as unknown as Api,
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
      exactPayment: {
        unsignedTransaction: '84a0a0f5f6',
        constructionData,
        isCollateralPreparation: false,
      },
    };
    store.setTransactionPendingState(true);
    const send = new Action();
    send.listen(store._sendMoney);
    await send.trigger();
    expect(store.isTransactionPending).toBe(false);
  });
});

describe('HardwareWalletsStore upstream payment lifetime', () => {
  const originalCip30Enabled = global.cip30Enabled;
  beforeEach(() => {
    global.cip30Enabled = false;
  });
  afterEach(() => {
    global.cip30Enabled = originalCip30Enabled;
    jest.restoreAllMocks();
    jest.useRealTimers();
  });

  const deferred = <T>() => {
    let resolve!: (value: T) => void;
    const promise = new Promise<T>((complete) => {
      resolve = complete;
    });
    return { promise, resolve };
  };
  const selection = (fee: number): CoinSelectionsResponse => ({
    ...coinSelection,
    fee: new BigNumber(fee),
    deposits: new BigNumber(0),
    depositsReclaimed: new BigNumber(0),
  });
  const payment: CoinSelectionsPaymentRequestType = {
    walletId: 'wallet',
    address: 'receiver',
    amount: 5_000_000,
    assets: [{ policy_id: 'policy', asset_name: 'asset', quantity: 2 }],
    metadata: {
      61284: { map: [{ k: { int: 1 }, v: { bytes: 'ab' } }] },
      61285: { map: [] },
    },
    withdrawal: 'self',
  };
  const fixture = () => {
    const select = jest.fn().mockResolvedValue(selection(1));
    const construct = jest.fn();
    const context = jest.fn();
    const submitDapp = jest.fn();
    const submit = jest.fn().mockResolvedValue({ id: 'transaction' });
    const release = jest.fn();
    const acquire = jest.fn().mockResolvedValue({ release });
    const store = new HardwareWalletsStore(
      {
        ada: {
          selectCoins: select,
          constructTransaction: construct,
          getDappTransactionContext: context,
          submitDappTransaction: submitDapp,
          createExternalTransaction: submit,
        },
        localStorage: {},
      } as unknown as Api,
      {} as ActionsMap,
      { sendEvent: jest.fn() } as unknown as AnalyticsTracker
    );
    store.stores = {
      wallets: {
        active: { id: 'wallet' },
        getWalletById: () => ({
          amount: new BigNumber(10),
          availableAmount: new BigNumber(8),
          reward: new BigNumber(1),
        }),
        refreshWalletsData: jest.fn(),
      },
      transactions: { acquireWalletSendLock: acquire },
    } as unknown as StoresMap;
    const device = jest
      .spyOn(
        store as unknown as {
          initiateDeviceTransaction: (
            params: { walletId: string | null | undefined },
            generation: number
          ) => Promise<void>;
        },
        'initiateDeviceTransaction'
      )
      .mockResolvedValue(undefined);
    return {
      store,
      select,
      construct,
      context,
      submitDapp,
      submit,
      release,
      acquire,
      device,
    };
  };

  it('keeps previews lease-free and captures upstream payment/assets/Catalyst metadata', async () => {
    const { store, select, construct, context, acquire } = fixture();
    const preview = await store.selectCoins(payment);
    store.updateTxSignRequest(preview);
    expect(acquire).not.toHaveBeenCalled();
    expect(construct).not.toHaveBeenCalled();
    expect(context).not.toHaveBeenCalled();
    expect(select).toHaveBeenCalledWith({
      walletId: 'wallet',
      walletBalance: new BigNumber(10),
      availableBalance: new BigNumber(9),
      rewardsBalance: new BigNumber(1),
      payments: {
        address: payment.address,
        amount: payment.amount,
        assets: payment.assets,
        withdrawal: 'self',
      },
      metadata: payment.metadata,
    });
    expect(preview.paymentRequest).toEqual(payment);
    expect(store.txSignRequest).toEqual({
      coinSelection: preview,
      paymentRequest: payment,
    });
    expect(store.txSignRequest.exactPayment).toBeUndefined();
  });

  it('selects a payment with the withdrawal the request carries', async () => {
    const { store, select } = fixture();
    const noWithdrawal = { ...payment, withdrawal: null };
    const preview = await store.selectCoins(noWithdrawal);
    expect(select.mock.calls[0][0].payments.withdrawal).toBeNull();
    expect(preview.paymentRequest.withdrawal).toBeNull();
  });

  it('reselects under the lease with the withdrawal the preview was priced with', async () => {
    const { store, select } = fixture();
    store.updateTxSignRequest(
      await store.selectCoins({ ...payment, withdrawal: null })
    );
    await store.initiateTransaction({ walletId: 'wallet' });
    expect(select).toHaveBeenCalledTimes(2);
    expect(
      select.mock.calls.map(([request]) => request.payments.withdrawal)
    ).toEqual([null, null]);
    expect(store.txSignRequest.paymentRequest.withdrawal).toBeNull();
  });

  it('sends the Ledger no withdrawal when the selection has none', async () => {
    const { store } = fixture();
    const request = jest
      .spyOn(signTransactionLedgerChannel, 'request')
      .mockRejectedValue(new Error('device stopped'));
    (store as any).stores.addresses = { _inspectAddress: jest.fn() };
    (store as any).stores.networkStatus = { absoluteSlotNumber: 100 };
    runInAction(() => {
      store.hardwareWalletsLocalDataRequest.result = {
        wallet: {
          extendedPublicKey: { publicKeyHex: '00', chainCodeHex: '00' },
        },
      } as unknown as HardwareWalletsStore['hardwareWalletsConnectionData'];
    });
    store.updateTxSignRequest({ ...selection(1), withdrawals: [] });
    await expect(
      store._signTransactionLedger('wallet', 'device')
    ).rejects.toThrow('device stopped');
    expect(request.mock.calls[0][0].withdrawals).toEqual([]);
  });

  it('reselects under the lease and retains it through external submission, including reset', async () => {
    const { store, select, acquire, release, device, submit, submitDapp } =
      fixture();
    store.updateTxSignRequest(await store.selectCoins(payment));
    select.mockImplementationOnce(() => {
      expect(acquire).toHaveBeenCalledTimes(1);
      expect(release).not.toHaveBeenCalled();
      return Promise.resolve(selection(2));
    });
    await store.initiateTransaction({ walletId: 'wallet' });
    expect(store.txSignRequest.coinSelection.fee).toEqual(new BigNumber(2));
    expect(device).toHaveBeenCalledTimes(1);
    expect(release).not.toHaveBeenCalled();
    const submitted = deferred<{ id: string }>();
    submit.mockReturnValueOnce(submitted.promise);
    store.signedTx = '84a0a0f5f6';
    const sending = store._sendMoney();
    expect(submit).toHaveBeenCalledWith({
      signedTransactionBlob: Buffer.from('84a0a0f5f6', 'hex'),
    });
    await store._resetTransaction({ cancelDeviceAction: false });
    expect(release).not.toHaveBeenCalled();
    submitted.resolve({ id: 'transaction' });
    await sending;
    expect(release).toHaveBeenCalledTimes(1);
    expect(submitDapp).not.toHaveBeenCalled();
    clearInterval(store.checkTransactionTimeInterval);
  });

  it('releases late lock acquisition after reset without selecting or touching a newer draft', async () => {
    const { store, select, acquire, release, device } = fixture();
    store.updateTxSignRequest(await store.selectCoins(payment));
    const lock = deferred<{ release: () => void }>();
    acquire.mockReturnValueOnce(lock.promise);
    const initiating = store.initiateTransaction({ walletId: 'wallet' });
    await store._resetTransaction({ cancelDeviceAction: false });
    const next = { ...selection(3), paymentRequest: payment };
    store.updateTxSignRequest(next);
    lock.resolve({ release });
    await initiating;
    expect(select).toHaveBeenCalledTimes(1);
    expect(device).not.toHaveBeenCalled();
    expect(store.txSignRequest.coinSelection.fee).toEqual(new BigNumber(3));
    expect(release).toHaveBeenCalledTimes(1);
  });

  it('ignores authoritative selection returning after reset without overwriting a newer draft', async () => {
    const { store, select, release, device } = fixture();
    store.updateTxSignRequest(await store.selectCoins(payment));
    const selected = deferred<CoinSelectionsResponse>();
    const selectionStarted = deferred<void>();
    select.mockImplementationOnce(() => {
      selectionStarted.resolve();
      return selected.promise;
    });
    const initiating = store.initiateTransaction({ walletId: 'wallet' });
    await selectionStarted.promise;
    await store._resetTransaction({ cancelDeviceAction: false });
    store.updateTxSignRequest({ ...selection(3), paymentRequest: payment });
    selected.resolve(selection(2));
    await initiating;
    expect(device).not.toHaveBeenCalled();
    expect(store.txSignRequest.coinSelection.fee).toEqual(new BigNumber(3));
    expect(release).toHaveBeenCalledTimes(1);
  });

  it('retains the delegation selection lease without reselecting or reacquiring on initiation', async () => {
    const { store, select, acquire, release, device } = fixture();
    await store.selectDelegationCoins({
      walletId: 'wallet',
      poolId: 'pool',
      delegationAction: 'join',
    });
    await store.initiateTransaction({ walletId: 'wallet' });
    expect(select).toHaveBeenCalledTimes(1);
    expect(acquire).toHaveBeenCalledTimes(1);
    expect(device).toHaveBeenCalledTimes(1);
    expect(release).not.toHaveBeenCalled();
    await store._resetTransaction({ cancelDeviceAction: false });
    expect(release).toHaveBeenCalledTimes(1);
  });

  it('rejects a queued delegation selection cancelled before lock acquisition', async () => {
    const { store, acquire, release, select, device } = fixture();
    const lock = deferred<{ release: () => void }>();
    acquire.mockReturnValueOnce(lock.promise);
    const selected = store.selectDelegationCoins({
      walletId: 'wallet',
      poolId: 'pool',
      delegationAction: 'quit',
    });
    await store._resetTransaction({ cancelDeviceAction: false });
    lock.resolve({ release });
    await expect(selected).rejects.toThrow('Hardware transaction cancelled');
    expect(select).not.toHaveBeenCalled();
    expect(device).not.toHaveBeenCalled();
    expect(release).toHaveBeenCalledTimes(1);
  });

  it('releases the lease on device setup failure and does not submit', async () => {
    const { store, release, device, submit } = fixture();
    store.updateTxSignRequest(await store.selectCoins(payment));
    device.mockRejectedValueOnce(new Error('Device rejected'));
    await expect(
      store.initiateTransaction({ walletId: 'wallet' })
    ).rejects.toThrow('Device rejected');
    expect(release).toHaveBeenCalledTimes(1);
    expect(submit).not.toHaveBeenCalled();
  });

  it('rejects direct exact helpers before any backend or device operation', async () => {
    const { store, construct, context, submitDapp, device } = fixture();
    const exact = store as unknown as ExactPaymentHelpers;
    await expect(
      exact.prepareExactPayment('wallet', constructionData)
    ).rejects.toThrow('Exact hardware transaction is unavailable');
    await expect(
      exact.submitExactPayment('wallet', 'signed', network, false)
    ).rejects.toThrow('Exact hardware transaction is unavailable');
    await expect(
      store.signDappTransaction('wallet', {
        status: 'empty',
      } as unknown as Parameters<
        HardwareWalletsStore['signDappTransaction']
      >[1])
    ).rejects.toThrow('Exact hardware transaction is unavailable');
    expect(construct).not.toHaveBeenCalled();
    expect(context).not.toHaveBeenCalled();
    expect(submitDapp).not.toHaveBeenCalled();
    expect(device).not.toHaveBeenCalled();
  });
});
