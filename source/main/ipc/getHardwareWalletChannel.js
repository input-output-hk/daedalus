// @flow
/* eslint-disable */
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
  GET_INIT_TREZOR_CONNECT_CHANNEL,
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
  handleInitTrezorConnectRendererRequest,
  handleInitTrezorConnectMainResponse,
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

const handleInitTrezorConnectChannel: MainIpcChannel<
  handleInitTrezorConnectRendererRequest,
  handleInitTrezorConnectMainResponse
> = new MainIpcChannel(GET_INIT_TREZOR_CONNECT_CHANNEL);


class EventObserver {
  constructor(props) {
    // INIT - 4
    console.debug('>>> Ledger::EventObserver');
    // $FlowFixMe
    this.mainWindow = props;
  }
  next = async (event) => {
    console.debug('>>> Ledger::EventObserver - nextEvent: ', event);
    const connectionChanged = event.type === 'add' || event.type === 'remove';
    // console.debug('>>> connectionChanged: ', connectionChanged);
    // const transport = await TransportNodeHid.create();
    // console.debug('>>> transport: ', transport);
    // const deviceConnection = new AppAda(transport);
    // console.debug('>>> deviceConnection: ', deviceConnection);
    // console.debug('>>> TRY info: ', transport.device);
    // const  deviceInfo = await transport.device.getDeviceInfo();
    // console.debug('>>> getDeviceInfo: ', deviceInfo);
    // try {
    //   const deviceSerial = await deviceConnection.getSerial();
    //   console.debug('>>> deviceSerial: ', deviceSerial);
    // } catch (e) {
    //   console.debug('>>> SERIAL error: ', e);
    // }

    //console.debug('>>> DEVICE serial: ', deviceSerial);

    if (connectionChanged) {
      getHardwareWalletConnectionChannel.send(
        {
          disconnected: event.type === 'remove',
          deviceType: 'ledger',
          deviceId: null, // Available only when Cardano APP opened
          deviceModel: event.deviceModel.id, // e.g. nanoS
          deviceName: event.deviceModel.productName, // e.g. Test Name
          path: event.device.path,
        },
        // $FlowFixMe
        this.mainWindow
      );
    }
  }
  error(e) {
    console.debug('>>> Ledger::EventObserver - error: ', e);
    throw e;
  }
  complete(res) {
    console.debug('>>> Ledger::EventObserver - complete: ', res);
  }
}

// INIT - 2
export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  console.debug('>>> Ledger:: INIT - handleHardwareWalletDevices');
  const handleCheckHardwareWalletDevices = async () => {
    // INIT - 3
    console.debug('>>> Ledger:: INIT - handleCheckHardwareWalletDevices');
    const observer = new EventObserver(mainWindow);
    // INIT - 5
    console.debug('>>> Ledger:: INIT - handleCheckHardwareWalletDevices - observer: ', observer);
    // @TODO - uncomment once Ledger enabled
    await TransportNodeHid.listen(observer);
  };

  return handleCheckHardwareWalletDevices;
};

// INIT - 1
export const handleHardwareWalletRequests = async (mainWindow) => {
  console.debug('>>> handleHardwareWalletRequests <<<');
  let deviceConnection = null;
  getHardwareWalletTransportChannel.onRequest(async request => {
    // INIT - 6
    const { isTrezor, devicePath } = request;
    console.debug('>>> ESTABLISH CONNECTION:  <<<, ', request);

    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      console.debug('>>> TEST - REQ: ', request);
      const test = await TrezorConnect.getFeatures({ device: { path: devicePath}});
      console.debug('>>> TEST 1: ', test);
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
        throw deviceFeatures.payload; // Error is in payload
      } catch (e) {
        console.debug('>>> ESTABLISH CONNECTION error: <<<', e);
        throw e;
      }
    }

    // @TODO - uncomment once Ledger enabled
    try {
      const transportList = await TransportNodeHid.list();
      console.debug('>>> LEDGER Connect <<<: ', {
        deviceConnection,
        transportList
      });

      let hw;
      if (
        !deviceConnection ||
        (deviceConnection &&
          deviceConnection.transport &&
          // $FlowFixMe
          deviceConnection.transport.disconnected)
      ) {
        console.debug('>>>  LEDGER Connect - NO Device connection instance <<<');
        if (transportList.length) {
          console.debug('>>>  LEDGER Connect - OPEN conn from list <<<');
          hw = await TransportNodeHid.open(transportList[0]);
          // hw = await TransportNodeHid.create();
        } else {
          console.debug('>>>  LEDGER Connect - CREATE new connection <<<');
          hw = await TransportNodeHid.create();
        }
      } else {
        console.debug('>>>  LEDGER Connect - device connection instance exists <<<');
        hw = deviceConnection.transport;
      }

      console.debug('>>> LEDGER BEGIN: ', {
        hw,
        deviceConnection,
      })

      if (!deviceConnection) {
        deviceConnection = new AppAda(hw);
      }

      const { deviceModel } = hw;
      if (deviceModel) {
        console.debug('>>> CONN ESTABLISHED: ', deviceModel);
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

  handleInitTrezorConnectChannel.onRequest(async () => {
    console.debug('>>> Trezor Init: ', TrezorConnect);
    TrezorConnect.on(TRANSPORT_EVENT, event => {
      console.debug('>>> TRANSPORT_EVENT: ', event);

      if (event.type === 'transport-error') {
        console.debug('>>> ECONNREFUSED <<<<');
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
        throw new Error(event.payload.error);
      }
    });
    TrezorConnect.on(DEVICE_EVENT, event => {
      console.debug('>>> DEVICE_EVENT: ', event);
      const connectionChanged = event.type === 'device-connect' || event.type === 'device-disconnect' || event.type === 'device-changed';
      const isAcquired = get(event, ['payload', 'type'], '') === 'acquired';
      const deviceError = get(event, ['payload', 'error']);

      if (deviceError) {
        throw new Error(deviceError);
        return;
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
    TrezorConnect.on(UI_EVENT, event => {
      console.debug('>>> UI_EVENT: ', event);
    });
    TrezorConnect.manifest({
      email: 'email@developer.com',
      appUrl: 'http://your.application.com',
    });
    const transport = TrezorConnect.init({
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
    .then(() => {
      console.debug('>>> TREZOR INIT - SUCCESS: ');
      return;
    })
    .catch(error => {
      console.debug('>>> TREZOR INIT - ERROR ', error);
      throw error;
    });
    return Promise.resolve({
      success: true,
    });
  });

  getCardanoAdaAppChannel.onRequest(async () => {
    const transportList = await TransportNodeHid.list();
    console.debug('>>> GET CARDANO APP <<<: ', {
      transportList,
      deviceConnection,
      transport: deviceConnection.transport,
      device: deviceConnection.transport.device,
    });
    // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel

    // @TODO - uncomment once Ledger enabled
    if (!deviceConnection) {
      try {
        console.debug('>>> GET CARDANO APP - open instances for: ', transportList[0])
        const newDeviceConnection = await TransportNodeHid.open(transportList[0]);
        deviceConnection = new AppAda(newDeviceConnection);
      } catch (e) {
        console.debug('>>> GET CARDANO APP - open instances FAILED: ', e);
        throw e;
      }
    }

    try {
      console.debug('>>> GET CARDANO APP - get version');
      const appVersion = await deviceConnection.getVersion();
      console.debug('>>> GET CARDANO APP - get version - RES: ', appVersion);

      console.debug('>>> Ledger - get Serial');
      const deviceSerial = await deviceConnection.getSerial();
      console.debug('>>> Ledger - get Serial - RES: ', deviceSerial);

      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      console.debug('>>> GET CARDANO APP - get version - FAILED: ', error);
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
      certificates,
    } = params;
    if (!TrezorConnect) {
      throw new Error('Device not connected!');
    }
    try {
      console.debug('>>> Signing REQ: ', params);
      const signedTransaction = await TrezorConnect.cardanoSignTransaction({
        inputs,
        outputs,
        fee,
        ttl,
        protocolMagic,
        networkId,
        certificates
      });
      console.debug('>>> Signing SUCC: ', signedTransaction);
      return Promise.resolve(signedTransaction);
    } catch (e) {
      console.debug('>>> Signing ERROR: ', e);
      throw e;
    }
  });
};
