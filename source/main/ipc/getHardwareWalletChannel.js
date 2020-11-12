// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import AppAda, { cardano, utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { BrowserWindow } from 'electron';
import TrezorConnect, {
  DEVICE_EVENT,
  TRANSPORT_EVENT,
  // $FlowFixMe
} from 'trezor-connect';
import { get } from 'lodash';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  SIGN_TRANSACTION_LEDGER_CHANNEL,
  SIGN_TRANSACTION_TREZOR_CHANNEL,
  GET_INIT_TREZOR_CONNECT_CHANNEL,
  GET_INIT_LEDGER_CONNECT_CHANNEL,
  DERIVE_XPUB_CHANNEL,
  RESET_ACTION_TREZOR_CHANNEL,
} from '../../common/ipc/api';

import type {
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse,
  getExtendedPublicKeyRendererRequest,
  getExtendedPublicKeyMainResponse,
  getCardanoAdaAppRendererRequest,
  getCardanoAdaAppMainResponse,
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse,
  signTransactionLedgerRendererRequest,
  signTransactionLedgerMainResponse,
  signTransactionTrezorRendererRequest,
  signTransactionTrezorMainResponse,
  handleInitTrezorConnectRendererRequest,
  handleInitTrezorConnectMainResponse,
  handleInitLedgerConnectRendererRequest,
  handleInitLedgerConnectMainResponse,
  resetTrezorActionRendererRequest,
  resetTrezorActionMainResponse,
  deriveXpubRendererRequest,
  deriveXpubMainResponse,
} from '../../common/ipc/api';

const getHardwareWalletTransportChannel: MainIpcChannel<
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse
> = new MainIpcChannel(GET_HARDWARE_WALLET_TRANSPORT_CHANNEL);

const getExtendedPublicKeyChannel: MainIpcChannel<
  getExtendedPublicKeyRendererRequest,
  getExtendedPublicKeyMainResponse
> = new MainIpcChannel(GET_EXTENDED_PUBLIC_KEY_CHANNEL);

const getCardanoAdaAppChannel: MainIpcChannel<
  getCardanoAdaAppRendererRequest,
  getCardanoAdaAppMainResponse
> = new MainIpcChannel(GET_CARDANO_ADA_APP_CHANNEL);

const getHardwareWalletConnectionChannel: MainIpcChannel<
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse
> = new MainIpcChannel(GET_HARDWARE_WALLET_CONNECTION_CHANNEL);

const signTransactionLedgerChannel: MainIpcChannel<
  signTransactionLedgerRendererRequest,
  signTransactionLedgerMainResponse
> = new MainIpcChannel(SIGN_TRANSACTION_LEDGER_CHANNEL);

const signTransactionTrezorChannel: MainIpcChannel<
  signTransactionTrezorRendererRequest,
  signTransactionTrezorMainResponse
> = new MainIpcChannel(SIGN_TRANSACTION_TREZOR_CHANNEL);

const resetTrezorActionChannel: MainIpcChannel<
  resetTrezorActionRendererRequest,
  resetTrezorActionMainResponse
> = new MainIpcChannel(RESET_ACTION_TREZOR_CHANNEL);

const handleInitTrezorConnectChannel: MainIpcChannel<
  handleInitTrezorConnectRendererRequest,
  handleInitTrezorConnectMainResponse
> = new MainIpcChannel(GET_INIT_TREZOR_CONNECT_CHANNEL);

const handleInitLedgerConnectChannel: MainIpcChannel<
  handleInitLedgerConnectRendererRequest,
  handleInitLedgerConnectMainResponse
> = new MainIpcChannel(GET_INIT_LEDGER_CONNECT_CHANNEL);

const deriveXpubChannel: MainIpcChannel<
  deriveXpubRendererRequest,
  deriveXpubMainResponse
> = new MainIpcChannel(DERIVE_XPUB_CHANNEL);

// Start Ledger listeners
// @TODO - uncomment once ledger instantiated
class EventObserver {
  constructor(props) {
    // INIT - 4
    // $FlowFixMe
    this.mainWindow = props;
  }
  next = async (event) => {
    console.debug('>>> Ledger NEXT: ', event);
    const connectionChanged = event.type === 'add' || event.type === 'remove';
    if (connectionChanged) {
      const device = get(event, 'device', {});
      const deviceModel = get(event, 'deviceModel', {});
      getHardwareWalletConnectionChannel.send(
        {
          disconnected: event.type === 'remove',
          deviceType: 'ledger',
          deviceId: null, // Available only when Cardano APP opened
          deviceModel: deviceModel.id, // e.g. nanoS
          deviceName: deviceModel.productName, // e.g. Test Name
          path: device.path,
        },
        // $FlowFixMe
        this.mainWindow
      );
    }
  };
  error(e) {
    console.debug('>>> Ledger NEXT error: ', e);
    throw e;
  }
  complete(event) {
    console.debug('>>> Ledger NEXT complete: ', event);
  }
}

// INIT - 2
export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  console.debug('>>> handleHardwareWalletDevices 1');
  const handleCheckHardwareWalletDevices = async () => {
    console.debug('>>> handleCheckHardwareWalletDevices 1');
    // INIT - 3
    const observer = new EventObserver(mainWindow);
    // INIT - 5
    // @TODO - uncomment once Ledger enabled
    await TransportNodeHid.listen(observer);
  };

  return handleCheckHardwareWalletDevices;
};

// INIT - 1
export const handleHardwareWalletRequests = async (
  mainWindow: BrowserWindow
) => {
  let deviceConnection = null;
  getHardwareWalletTransportChannel.onRequest(async (request) => {
    console.debug('>>> getHardwareWalletTransportChannel: ', request);
    // INIT - 6
    const { isTrezor, devicePath } = request;
    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      console.debug('>>> getHardwareWalletTransportChannel:: TREZOR ');
      try {
        deviceFeatures = await TrezorConnect.getFeatures({
          device: { path: devicePath },
        });
        if (deviceFeatures && deviceFeatures.success) {
          const {
            major_version: majorVersion,
            minor_version: minorVersion,
            patch_version: patchVersion,
            device_id: deviceId,
            model,
            label,
          } = deviceFeatures.payload;
          const firmwareVersion = `${majorVersion}.${minorVersion}.${patchVersion}`;
          return Promise.resolve({
            deviceId,
            deviceType: 'trezor',
            deviceModel: model, // e.g. "1" or "T"
            deviceName: label,
            path: devicePath,
            firmwareVersion,
          });
        }
        throw deviceFeatures.payload; // Error is in payload
      } catch (e) {
        throw e;
      }
    }

    // @TODO - uncomment once Ledger enabled
    try {
      console.debug('>>> getHardwareWalletTransportChannel:: LEDGER: ', { TransportNodeHid });
      const transportList = await TransportNodeHid.list();
      let hw;
      if (
        !deviceConnection ||
        (deviceConnection &&
          deviceConnection.transport &&
          // $FlowFixMe
          deviceConnection.transport.disconnected)
      ) {
        console.debug('>>> getHardwareWalletTransportChannel:: LEDGER case 1');
        if (transportList.length) {
          console.debug('>>> getHardwareWalletTransportChannel:: LEDGER case 1.1');
          hw = await TransportNodeHid.open(transportList[0]);
          // hw = await TransportNodeHid.create();
        } else {
          console.debug('>>> getHardwareWalletTransportChannel:: LEDGER case 1.2');
          hw = await TransportNodeHid.create();
        }
      } else {
        console.debug('>>> getHardwareWalletTransportChannel:: LEDGER case 2');
        hw = deviceConnection.transport;
      }

      if (!deviceConnection) {
        deviceConnection = new AppAda(hw);
        console.debug('>>> getHardwareWalletTransportChannel:: LEDGER case get appAda: ', deviceConnection);
      }

      const { deviceModel } = hw;
      if (deviceModel) {
        console.debug('>>> getHardwareWalletTransportChannel:: LEDGER case RESPONSE: ', hw);
        const { id, productName } = deviceModel;
        return Promise.resolve({
          deviceId: null, // @TODO - to be defined
          deviceType: 'ledger',
          deviceModel: id, // e.g. nanoS
          deviceName: productName, // e.g. Ledger Nano S
          path: null,
          firmwareVersion: null,
        });
      }
      throw new Error('Missing device info');
    } catch (error) {
      throw error;
    }
  });

  handleInitTrezorConnectChannel.onRequest(async () => {
    console.debug('>>> INIT TREZOR <<<');
    // Remove all listeners if exist - e.g. on app refresh
    TrezorConnect.removeAllListeners();
    // Initialize new device listeners
    TrezorConnect.on(TRANSPORT_EVENT, (event) => {
      if (event.type === 'transport-error') {
        // Send Transport error to Renderer
        getHardwareWalletConnectionChannel.send(
          {
            error: {
              payload: event.payload,
            },
          },
          // $FlowFixMe
          mainWindow
        );
      }
    });
    TrezorConnect.on(DEVICE_EVENT, (event) => {
      const connectionChanged =
        event.type === 'device-connect' ||
        event.type === 'device-disconnect' ||
        event.type === 'device-changed';
      const isAcquired = get(event, ['payload', 'type'], '') === 'acquired';
      const deviceError = get(event, ['payload', 'error']);

      if (deviceError) {
        throw new Error(deviceError);
      }

      if (connectionChanged && isAcquired) {
        getHardwareWalletConnectionChannel.send(
          {
            disconnected: event.type === 'device-disconnect',
            deviceType: 'trezor',
            deviceId: event.payload.id, // 58E026E1F5CF549198EDCC35
            deviceModel: event.payload.features.model, // e.g. T
            deviceName: event.payload.label, // e.g. Test Name
            path: event.payload.path,
          },
          // $FlowFixMe
          mainWindow
        );
      }
    });
    TrezorConnect.manifest({
      email: 'email@developer.com',
      appUrl: 'http://your.application.com',
    });
    TrezorConnect.init({
      popup: false, // render your own UI
      webusb: false, // webusb is not supported in electron
      debug: true, // see what's going on inside connect
      // lazyLoad: true, // set to "false" (default) if you want to start communication with bridge on application start (and detect connected device right away)
      // set it to "true", then trezor-connect will not be initialized until you call some TrezorConnect.method()
      // this is useful when you don't know if you are dealing with Trezor user
      manifest: {
        email: 'email@developer.com', // @TODO
        appUrl: 'http://your.application.com', // @TODO
      },
    })
      .then(() => {})
      .catch((error) => {
        throw error;
      });
  });

  handleInitLedgerConnectChannel.onRequest(async () => {
    console.debug('>>> INIT LEDGER <<<');
    console.debug('>>> handleHardwareWalletDevices 222');
    // INIT - 3
    const observer = new EventObserver(mainWindow);
    // INIT - 5
    // @TODO - uncomment once Ledger enabled
    const listener = await TransportNodeHid.listen(observer);
  });

  deriveXpubChannel.onRequest(async () => {
    console.debug('>>> Derive Xpub <<<');
  });

  getCardanoAdaAppChannel.onRequest(async () => {

    if (!deviceConnection) {
      console.debug('>>> NO DEVICE CONN <<');
      try {
        const transportList = await TransportNodeHid.list();
        console.debug('>>> transportList: ', transportList);
        const transport = await TransportNodeHid.open(transportList[0]);
        console.debug('>>> transport: ', transport);
        deviceConnection = new AppAda(transport);
      } catch (e) {
        console.debug('>>> ERROR 1: ', e);
        throw e;
      }
    }

    try {
      const appVersion = await deviceConnection.getVersion();
      console.debug('>>> getCardanoAdaAppChannel:: appVersion ', appVersion);
      const deviceSerial = await deviceConnection.getSerial();
      console.debug('>>> getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      console.debug('>>> ERROR2 : ', error);
      if (error.name === 'DisconnectedDevice') {
        // Set old connection to null and force reinitialization once method called again
        deviceConnection = null;
      }
      throw error;
    }
    return;



    const transportList = await TransportNodeHid.list();
    console.debug('>>> getCardanoAdaAppChannel: ', {transportList, deviceConnection});
    // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel
    if (!deviceConnection) {
      console.debug('>>> getCardanoAdaAppChannel:: NO device connection');
      try {
        // const newDeviceConnection = await TransportNodeHid.open(
        //   transportList[0]
        // );
        const newDeviceConnection = await TransportNodeHid.create();
        console.debug('>>> getCardanoAdaAppChannel:: newDeviceConnection: ', {newDeviceConnection});
        deviceConnection = new AppAda(newDeviceConnection);
        console.debug('>>> getCardanoAdaAppChannel:: newDeviceConnection - APP ADA instance: ', {deviceConnection});
      } catch (e) {
        console.debug('>>> getCardanoAdaAppChannel::ERROR 1 ', e);
        throw e;
      }
    }
    console.debug('>>> DC: ', deviceConnection);

    // deviceConnection = await TransportNodeHid.create()
    // console.debug('>>> NWEW 2:', deviceConnection);

    // const aa = await TransportNodeHid.create()
    // deviceConnection = new AppAda(aa);

    try {
      const appVersion = await deviceConnection.getVersion();
      console.debug('>>> getCardanoAdaAppChannel:: appVersion ', appVersion);
      const deviceSerial = await deviceConnection.getSerial();
      console.debug('>>> getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      console.debug('>>> getCardanoAdaAppChannel::ERROR 2 ', error);
      throw error;
    }
  });

  getExtendedPublicKeyChannel.onRequest(async (params) => {
    // params example:
    // { path: "1852'/1815'/0'", isTrezor: false, devicePath: null }
    console.debug('>>> getExtendedPublicKeyChannel: ', params);
    const { path, isTrezor, devicePath } = params;
    try {
      if (isTrezor) {
        // Check if Trezor instantiated
        const deviceFeatures = await TrezorConnect.getFeatures({
          device: { path: devicePath },
        });
        if (deviceFeatures.success) {
          // trezorConnected = true;
          const extendedPublicKeyResponse = await TrezorConnect.cardanoGetPublicKey(
            {
              path: `m/${path}`,
              showOnTrezor: true,
            }
          );
          if (!extendedPublicKeyResponse.success) {
            throw extendedPublicKeyResponse.payload;
          }
          const extendedPublicKey = get(
            extendedPublicKeyResponse,
            ['payload', 'node'],
            {}
          );
          return Promise.resolve({
            publicKeyHex: extendedPublicKey.public_key,
            chainCodeHex: extendedPublicKey.chain_code,
          });
        }
        throw new Error('Trezor device not connected');
      }

      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }
      console.debug('>>> PATH: ', {path, toPath: cardano.str_to_path(path)});
      const extendedPublicKey = await deviceConnection.getExtendedPublicKey(
        cardano.str_to_path(path)
      );
      const deviceSerial = await deviceConnection.getSerial();
      return Promise.resolve({
        publicKeyHex: extendedPublicKey.publicKeyHex,
        chainCodeHex: extendedPublicKey.chainCodeHex,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      throw error;
    }
  });

  signTransactionLedgerChannel.onRequest(async (params) => {
    const {
      inputs,
      outputs,
      protocolMagic,
      fee,
      ttl,
      networkId,
      certificates,
      withdrawals,
      metadataHashHex,
    } = params;

    try {
      if (!deviceConnection) {
        throw new Error('Device not connected!');
      }
      const signedTransaction = await deviceConnection.signTransaction(
        networkId,
        protocolMagic,
        inputs,
        outputs,
        fee,
        ttl,
        certificates,
        withdrawals,
        metadataHashHex
      );
      return Promise.resolve(signedTransaction);
    } catch (e) {
      throw e;
    }
  });

  signTransactionTrezorChannel.onRequest(async (params) => {
    const {
      inputs,
      outputs,
      protocolMagic,
      fee,
      ttl,
      networkId,
      certificates,
      devicePath,
    } = params;

    if (!TrezorConnect) {
      throw new Error('Device not connected!');
    }

    try {
      const dataToSign = {
        inputs,
        outputs,
        fee,
        ttl,
        protocolMagic,
        networkId,
        certificates,
      };
      const signedTransaction = await TrezorConnect.cardanoSignTransaction({
        device: { path: devicePath },
        ...dataToSign,
      });
      return Promise.resolve(signedTransaction);
    } catch (e) {
      throw e;
    }
  });

  resetTrezorActionChannel.onRequest(async () => {
    TrezorConnect.cancel('Method_Cancel');
  });
};
