// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import AppAda, { cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
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
  resetTrezorActionRendererRequest,
  resetTrezorActionMainResponse,
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

// Start Ledger listeners
// @TODO - uncomment once ledger instantiated
class EventObserver {
  constructor(props) {
    // INIT - 4
    // $FlowFixMe
    this.mainWindow = props;
  }
  next = async (event) => {
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
    throw e;
  }
  complete() {}
}

// INIT - 2
export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  const handleCheckHardwareWalletDevices = async () => {
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
    // INIT - 6
    const { isTrezor, devicePath } = request;
    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
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
      const transportList = await TransportNodeHid.list();
      let hw;
      if (
        !deviceConnection ||
        (deviceConnection &&
          deviceConnection.transport &&
          // $FlowFixMe
          deviceConnection.transport.disconnected)
      ) {
        if (transportList.length) {
          hw = await TransportNodeHid.open(transportList[0]);
          // hw = await TransportNodeHid.create();
        } else {
          hw = await TransportNodeHid.create();
        }
      } else {
        hw = deviceConnection.transport;
      }

      if (!deviceConnection) {
        deviceConnection = new AppAda(hw);
      }

      const { deviceModel } = hw;
      if (deviceModel) {
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

  getCardanoAdaAppChannel.onRequest(async () => {
    const transportList = await TransportNodeHid.list();
    // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel
    if (!deviceConnection) {
      try {
        const newDeviceConnection = await TransportNodeHid.open(
          transportList[0]
        );
        deviceConnection = new AppAda(newDeviceConnection);
      } catch (e) {
        throw e;
      }
    }

    try {
      const appVersion = await deviceConnection.getVersion();
      const deviceSerial = await deviceConnection.getSerial();
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      throw error;
    }
  });

  getExtendedPublicKeyChannel.onRequest(async (params) => {
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
      const extendedPublicKey = await deviceConnection.getExtendedPublicKey(
        cardano.str_to_path(path)
      );
      return Promise.resolve({
        publicKeyHex: extendedPublicKey.publicKeyHex,
        chainCodeHex: extendedPublicKey.chainCodeHex,
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
