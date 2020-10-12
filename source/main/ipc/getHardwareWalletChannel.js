// @flow
/* eslint-disable no-console */
// @TODO - enable console once connecting logic refined
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import AppAda, { cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { BrowserWindow } from 'electron';
// $FlowFixMe
import TrezorConnect, { DEVICE_EVENT, TRANSPORT_EVENT, UI_EVENT } from 'trezor-connect';
import { get } from 'lodash';
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  SIGN_TRANSACTION_LEDGER_CHANNEL,
  SIGN_TRANSACTION_TREZOR_CHANNEL,
} from '../../common/ipc/api';

import type { IpcSender } from '../../common/ipc/lib/IpcChannel';

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

class EventObserver {
  constructor(props) {
    // $FlowFixMe
    this.mainWindow = props;
  }
  next(eventText) {
    if (eventText.type === 'remove') {
      getHardwareWalletConnectionChannel.send(
        { disconnected: true },
        // $FlowFixMe
        this.mainWindow
      );
    }
  }
  error(e) {
    throw e;
  }
  complete() {}
}

// SETUP trezor-connect
export const handleInitTrezorConnect = (sender: IpcSender) => {
  console.debug('>>> Trezor Init: ', TrezorConnect);
  const initTrezorConnect = async () => {
    TrezorConnect.on(TRANSPORT_EVENT, event => {
      console.debug('>>> TRANSPORT_EVENT: ', event);
    });
    TrezorConnect.on(DEVICE_EVENT, event => {
      console.debug('>>> DEVICE_EVENT: ', event);
      const connectionChanged = event.type === 'device-connect' || event.type === 'device-disconnect' || event.type === 'device-changed';
      if (connectionChanged) {
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
          sender
        );
      }
    });
    TrezorConnect.on(UI_EVENT, event => {
      console.debug('>>> UI_EVENT: ', event);
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
    .then(res => {
      console.debug('>>> TREZOR INIT - SUCCESS: ', res);
      sender.send('trezor-connect', 'TrezorConnect is ready!');
    })
    .catch(error => {
      console.debug('>>> TREZOR INIT - ERROR ', error);
      sender.send('trezor-connect', `TrezorConnect init error: ${error}`);
    });
  };

  return initTrezorConnect;
};

export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  const handleCheckHardwareWalletDevices = async () => {
    const observer = new EventObserver(mainWindow);
    await TransportNodeHid.listen(observer);
  };

  return handleCheckHardwareWalletDevices;
};

export const handleHardwareWalletRequests = async (mainWindow) => {
  let deviceConnection = null;
  getHardwareWalletTransportChannel.onRequest(async request => {
    const { isTrezor, devicePath } = request;


    // const test = await TrezorConnect.getFeatures();
    console.debug('>>> TEST - REQ: ', request);
    const test = await TrezorConnect.getFeatures({ device: { path: devicePath}});
    console.debug('>>> TEST 1: ', test);


    return Promise.resolve({
      deviceId: test.payload.device_id,
      deviceType: 'trezor',
      deviceModel: test.payload.model, // e.g. "1" or "T"
      deviceName: test.payload.label,
      path: devicePath,
    });

    // console.debug('>>> TrezorConnect METHODS: ', TrezorConnect);
    // return;

    console.debug('>>> ESTABLISH CONNECTION:  <<<, ', request);
    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      try {
        console.debug('>>> ESTABLISH CONNECTION Trezor');
        deviceFeatures = await TrezorConnect.getFeatures({ device: { path: devicePath } });
        console.debug('>>> Trezor Connected: ', deviceFeatures);
        if (deviceFeatures && deviceFeatures.success) {
          return Promise.resolve({
            deviceId: deviceFeatures.payload.device_id,
            deviceType: 'trezor',
            deviceModel: deviceFeatures.payload.model, // e.g. "1" or "T"
            deviceName: deviceFeatures.payload.label,
            path: devicePath,
          });
        }
      } catch (e) {
        console.debug('>>> ESTABLISH CONNECTION error: <<<', e);
        throw new Error('Establishing Trezor connection failed');
      }
    }

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
          hw = await TransportNodeHid.create();
        } else {
          hw = await TransportNodeHid.create();
        }
      } else {
        hw = deviceConnection.transport;
      }
      deviceConnection = new AppAda(hw);
      const { deviceModel } = hw;
      if (deviceModel) {
        const { id, productName } = deviceModel;
        return Promise.resolve({
          deviceId: null, // @TODO - to be defined
          deviceType: 'ledger',
          deviceModel: id, // e.g. nanoS
          deviceName: productName, // e.g. Ledger Nano S
        });
      }
      throw new Error('Missing device info');
    } catch (error) {
      throw error;
    }
  });

  getCardanoAdaAppChannel.onRequest(async () => {
    const transportList = await TransportNodeHid.list();
    // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel
    try {
      const newConnection = await TransportNodeHid.open(transportList[0]);
      deviceConnection = new AppAda(newConnection);
    } catch (e) {
      throw e;
    }
    try {
      const appVersion = await deviceConnection.getVersion();
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
      });
    } catch (error) {
      throw error;
    }
  });

  getExtendedPublicKeyChannel.onRequest(async params => {
    const { path, isTrezor, devicePath } = params;
    // let trezorConnected = false;

    console.debug('>>> getExtendedPublicKeyChannel: ', params);

    try {
      if (isTrezor) {
        // Check if Trezor instantiated
        const deviceFeatures = await TrezorConnect.getFeatures({ device: { path: devicePath } });
        console.debug('>>> Trezor - getExtendedPublicKey::Device Features: ', deviceFeatures);
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

      const extendedPublicKey = await deviceConnection.getExtendedPublicKey(cardano.str_to_path(path));
      return Promise.resolve({
        publicKeyHex: extendedPublicKey.publicKeyHex,
        chainCodeHex: extendedPublicKey.chainCodeHex,
      });
    } catch (error) {
      throw error;
    }
  });


  signTransactionLedgerChannel.onRequest(async params => {
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
        metadataHashHex,
      );
      return Promise.resolve(signedTransaction);
    } catch (e) {
      throw e;
    }
  });

  signTransactionTrezorChannel.onRequest(async params => {
    const {
      inputs,
      outputs,
      protocolMagic,
      fee,
      ttl,
      networkId,
    } = params;
    if (!TrezorConnect) {
      throw new Error('Device not connected!');
    }
    try {
      const signedTransaction = await TrezorConnect.cardanoSignTransaction({
        inputs,
        outputs,
        fee,
        ttl,
        protocolMagic,
        networkId
      });
      return Promise.resolve({ serializedTx: signedTransaction.payload.serializedTx });
    } catch (e) {
      throw e;
    }
  });
};
