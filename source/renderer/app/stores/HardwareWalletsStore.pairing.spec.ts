jest.mock('@trezor/connect', () => ({}));
jest.mock('../utils/logging', () => ({
  logger: {
    debug: jest.fn(),
    error: jest.fn(),
    info: jest.fn(),
    warn: jest.fn(),
  },
}));

import HardwareWalletsStore from './HardwareWalletsStore';
import {
  DeviceModels,
  DeviceTypes,
} from '../../../common/types/hardware-wallets.types';
import { HwDeviceStatuses } from '../domains/Wallet';
import type { Api } from '../api';
import type { ActionsMap } from '../actions';
import type { AnalyticsTracker } from '../analytics';

const extendedPublicKey = {
  publicKeyHex: '11'.repeat(32),
  chainCodeHex: '22'.repeat(32),
  deviceId: 'flex',
};

describe('HardwareWalletsStore wallet pairing', () => {
  it('waits for a wallet name before creating an unrecognized wallet', async () => {
    const request = jest.fn();
    const store = new HardwareWalletsStore(
      ({
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
      } as unknown) as Api,
      {} as ActionsMap,
      {} as AnalyticsTracker
    );
    store.transportDevice = {
      deviceId: 'flex',
      deviceType: DeviceTypes.LEDGER,
      deviceModel: DeviceModels.LEDGER_FLEX,
      deviceName: 'Ledger Flex',
      path: '/dev/hidraw9',
    };
    store.isTransactionInitiated = false;
    store._requestExtendedPublicKey = jest
      .fn()
      .mockResolvedValue(extendedPublicKey);
    store._findAssociatedWalletByExtendedPublicKey = jest
      .fn()
      .mockResolvedValue(null);
    store._createNewWalletForRecognizedPendingDevice = jest.fn();

    await store._identifyAndHandleAssociatedWallet({});

    expect(store.hwDeviceStatus).toBe(HwDeviceStatuses.READY);
    expect(store.extendedPublicKey).toEqual(extendedPublicKey);
    expect(
      store._createNewWalletForRecognizedPendingDevice
    ).not.toHaveBeenCalled();

    await store.createHardwareWallet('Ledger Flex wallet');

    expect(
      store._createNewWalletForRecognizedPendingDevice
    ).toHaveBeenCalledWith({
      extendedPublicKey,
      walletName: 'Ledger Flex wallet',
    });
  });
});
