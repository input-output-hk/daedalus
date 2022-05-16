import { BrowserWindow } from 'electron';
import modulik from 'modulik';
import { environment } from '../environment';
import { HardwareWalletChannels } from './createHardwareWalletIPCChannels';
import type * as hardwareWallets from './hardwareWallets';

const hwLogicModulik = modulik<typeof hardwareWallets>('./hardwareWallets', {
  disabled: environment.isProduction,
  transpiler: 'typescript',
});

export const handleHardwareWalletRequests = async (
  mainWindow: BrowserWindow,
  {
    getHardwareWalletTransportChannel,
    getExtendedPublicKeyChannel,
    getCardanoAdaAppChannel,
    getHardwareWalletConnectionChannel,
    signTransactionLedgerChannel,
    signTransactionTrezorChannel,
    resetTrezorActionChannel,
    handleInitTrezorConnectChannel,
    handleInitLedgerConnectChannel,
    deriveXpubChannel,
    deriveAddressChannel,
    showAddressChannel,
    waitForLedgerDevicesToConnectChannel,
  }: HardwareWalletChannels
) => {
  hwLogicModulik.on('ready', () => {
    mainWindow.reload();
  });

  const hwLogic = await hwLogicModulik.module;

  const sendHardwareWalletConnectionMessage = async (data: any) => {
    // @ts-ignore
    getHardwareWalletConnectionChannel.send(data, mainWindow);
  };

  waitForLedgerDevicesToConnectChannel.onRequest(
    // @ts-ignore
    hwLogic.onWaitForLedgerDevicesToConnectChannelRequest
  );

  getHardwareWalletTransportChannel.onRequest(
    hwLogic.onGetHardwareWalletTransportChannelRequest
  );

  handleInitTrezorConnectChannel.onRequest(async () => {
    hwLogic.onHandleInitTrezorConnectChannelRequest({
      sendHardwareWalletConnectionMessage,
    });
  });

  handleInitLedgerConnectChannel.onRequest(() =>
    hwLogic.onHandleInitLedgerConnectChannelRequest({
      sendHardwareWalletConnectionMessage,
    })
  );

  deriveXpubChannel.onRequest(hwLogic.onDderiveXpubChannelRequest);

  deriveAddressChannel.onRequest(hwLogic.onDeriveAddressChannelRequest);

  showAddressChannel.onRequest(hwLogic.onShowAddressChannelRequest);

  getCardanoAdaAppChannel.onRequest(hwLogic.onGetCardanoAdaAppChannelRequest);

  getExtendedPublicKeyChannel.onRequest(
    hwLogic.onGetExtendedPublicKeyChannelRequest
  );

  signTransactionLedgerChannel.onRequest(
    hwLogic.onSignTransactionLedgerChannelRequest
  );

  signTransactionTrezorChannel.onRequest(
    // @ts-ignore
    hwLogic.onSignTransactionTrezorChannelRequest
  );

  resetTrezorActionChannel.onRequest(hwLogic.onResetTrezorActionChannelRequest);
};

export const isLedgerListenerActive = async () => {
  const hwLogic = await hwLogicModulik.module;
  return hwLogic.isLedgerListenerActive();
};

export const closeLedgerListener = async () => {
  const hwLogic = await hwLogicModulik.module;
  return hwLogic.closeLedgerListener();
};
