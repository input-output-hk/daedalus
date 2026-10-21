import HardwareWalletsStore from './HardwareWalletsStore';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../common/types/hardware-wallets.types';
import Wallet, { HwDeviceStatuses } from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';
import type { StoresMap } from './index';
import type { HardwareWalletsLocalData } from '../types/localDataTypes';

jest.mock('@trezor/connect', () => ({}));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

const extendedPublicKey = {
  publicKeyHex: '11'.repeat(32),
  chainCodeHex: '22'.repeat(32),
  deviceId: 'flex',
};

const createStore = () => {
  const request = jest.fn();
  const createWallet = jest.fn().mockResolvedValue(undefined);
  const store = new HardwareWalletsStore(
    {
      ada: {
        selectCoins: request,
        createExternalTransaction: request,
        getPublicKey: request,
        constructAddress: request,
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
    {
      wallets: { createHardwareWallet: { trigger: createWallet } },
    } as unknown as ActionsMap,
    {} as AnalyticsTracker
  );
  store.transportDevice = {
    deviceId: 'flex',
    deviceType: DeviceTypes.LEDGER,
    deviceModel: DeviceModels.LEDGER_FLEX,
    deviceName: 'Ledger Flex',
    path: '/dev/hidraw9',
  };
  store.isWalletPairingInitiated = true;
  store._requestExtendedPublicKey = jest
    .fn()
    .mockResolvedValue(extendedPublicKey);
  const findAssociatedWallet = store._findAssociatedWalletByExtendedPublicKey;
  store._findAssociatedWalletByExtendedPublicKey = jest
    .fn()
    .mockResolvedValue(null);
  store.cleanUpPendingDevices = jest.fn().mockResolvedValue([]);
  store._refreshHardwareWalletsLocalData = jest
    .fn()
    .mockResolvedValue(undefined);
  store._refreshHardwareWalletDevices = jest.fn().mockResolvedValue(undefined);
  // These request results are the persisted device boundary, not a pairing helper.
  store.hardwareWalletDevicesRequest.result = {};
  store.resetInitializedConnection = jest.fn().mockResolvedValue(undefined);
  return { store, createWallet, findAssociatedWallet };
};

const deferred = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (error: Error) => void;
  const promise = new Promise<T>((accept, fail) => {
    resolve = accept;
    reject = fail;
  });
  return { promise, resolve, reject };
};

describe('HardwareWalletsStore wallet pairing', () => {
  const originalGate = global.cip30Enabled;
  afterEach(() => {
    global.cip30Enabled = originalGate;
  });

  it('automatically creates an upstream wallet with its device name', async () => {
    global.cip30Enabled = false;
    const { store, createWallet } = createStore();
    await store._identifyAndHandleAssociatedWallet({ path: '/dev/hidraw9' });
    expect(createWallet).toHaveBeenCalledWith({
      walletName: 'Ledger Flex',
      extendedPublicKey,
      device: { ...store.transportDevice, firmwareVersion: null },
    });
    expect(store.isWalletPairingInitiated).toBe(false);
    expect(store.cleanUpPendingDevices).toHaveBeenCalledTimes(1);
  });

  it('uses the historical default name when the device has no name', async () => {
    global.cip30Enabled = false;
    const { store, createWallet } = createStore();
    store.transportDevice.deviceName = '';
    await store._identifyAndHandleAssociatedWallet({});
    expect(createWallet.mock.calls[0][0].walletName).toBe('Hardware Wallet');
  });

  it('requires explicit named pairing on the pilot', async () => {
    global.cip30Enabled = true;
    const { store, createWallet } = createStore();
    await store._identifyAndHandleAssociatedWallet({});
    expect(store.hwDeviceStatus).toBe(HwDeviceStatuses.READY);
    expect(store.extendedPublicKey).toEqual(extendedPublicKey);
    expect(createWallet).not.toHaveBeenCalled();
    await store.createHardwareWallet('Ledger Flex wallet');
    expect(createWallet.mock.calls[0][0].walletName).toBe('Ledger Flex wallet');
  });

  it.each([false, true])(
    'reuses an existing wallet instead of creating a duplicate (pilot %s)',
    async (pilot) => {
      global.cip30Enabled = pilot;
      const { store, createWallet, findAssociatedWallet } = createStore();
      const associatedWallet = { id: 'existing-wallet' } as Wallet;
      store.configure({
        wallets: {
          getWalletById: (id: string) =>
            id === associatedWallet.id ? associatedWallet : null,
        },
      } as unknown as StoresMap);
      store.hardwareWalletsLocalDataRequest.result = {
        existing: { id: associatedWallet.id, extendedPublicKey },
      } as unknown as HardwareWalletsLocalData;
      store._findAssociatedWalletByExtendedPublicKey = findAssociatedWallet;
      const handleExisting = jest.fn().mockResolvedValue(undefined);
      store._storeWalletDataInLocalStorageAndHandleTransactionOrAddressVerificationOrRouting =
        handleExisting;
      await store._identifyAndHandleAssociatedWallet({});
      expect(handleExisting).toHaveBeenCalledWith(
        expect.objectContaining({ associatedWallet, extendedPublicKey })
      );
      expect(createWallet).not.toHaveBeenCalled();
    }
  );

  it.each([false, true])(
    'ignores a key returned after cancellation (pilot %s)',
    async (pilot) => {
      global.cip30Enabled = pilot;
      const { store, createWallet } = createStore();
      const key = deferred<typeof extendedPublicKey>();
      store._requestExtendedPublicKey = jest.fn().mockReturnValue(key.promise);
      const identifying = store._identifyAndHandleAssociatedWallet({});
      await store.resetWalletPairing();
      key.resolve(extendedPublicKey);
      await identifying;
      expect(createWallet).not.toHaveBeenCalled();
      expect(store.extendedPublicKey).toBeNull();
      expect(
        store._findAssociatedWalletByExtendedPublicKey
      ).not.toHaveBeenCalled();
    }
  );

  it('does not let a stale lookup change a restarted pairing', async () => {
    global.cip30Enabled = false;
    const { store, createWallet } = createStore();
    const lookup = deferred<Wallet | null>();
    store._findAssociatedWalletByExtendedPublicKey = jest
      .fn()
      .mockReturnValue(lookup.promise);
    const identifying = store._identifyAndHandleAssociatedWallet({});
    await Promise.resolve();
    await store.resetWalletPairing();
    store.establishHardwareWalletConnection = jest.fn();
    store.initiateWalletPairing();
    lookup.resolve(null);
    await identifying;
    expect(createWallet).not.toHaveBeenCalled();
    expect(store.isWalletPairingInitiated).toBe(true);
  });

  it('does not reset a newer pairing when an old key request fails', async () => {
    const { store } = createStore();
    const key = deferred<typeof extendedPublicKey>();
    store._requestExtendedPublicKey = jest.fn().mockReturnValue(key.promise);
    const identifying = store._identifyAndHandleAssociatedWallet({});
    await store.resetWalletPairing();
    store.establishHardwareWalletConnection = jest.fn();
    store.initiateWalletPairing();
    key.reject(new Error('device disconnected'));
    await identifying;
    expect(store.isWalletPairingInitiated).toBe(true);
  });

  it('does not submit duplicate Pair clicks while wallet creation is pending', async () => {
    global.cip30Enabled = true;
    const { store, createWallet } = createStore();
    const creation = deferred<void>();
    createWallet.mockReturnValue(creation.promise);
    await store._identifyAndHandleAssociatedWallet({});
    const first = store.createHardwareWallet('Named wallet');
    await store.createHardwareWallet('Named wallet');
    expect(createWallet).toHaveBeenCalledTimes(1);
    creation.resolve();
    await first;
  });

  it('does not clear a newer pairing when an earlier wallet creation completes', async () => {
    global.cip30Enabled = false;
    const { store, createWallet } = createStore();
    const creation = deferred<void>();
    createWallet.mockReturnValue(creation.promise);
    const identifying = store._identifyAndHandleAssociatedWallet({});
    await Promise.resolve();
    await Promise.resolve();
    await store.resetWalletPairing();
    store.establishHardwareWalletConnection = jest.fn();
    store.initiateWalletPairing();
    creation.resolve();
    await identifying;
    expect(store.isWalletPairingInitiated).toBe(true);
    expect(store.resetInitializedConnection).not.toHaveBeenCalled();
  });
});
