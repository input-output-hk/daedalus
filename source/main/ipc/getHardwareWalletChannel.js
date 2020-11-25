// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import AppAda, {
  cardano,
  utils,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { BrowserWindow } from 'electron';
import TrezorConnect, {
  DEVICE_EVENT,
  TRANSPORT_EVENT,
  // $FlowFixMe
} from 'trezor-connect';
import { get, omit, last, includes } from 'lodash';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
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

import { logger } from '../utils/logging';

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

let devicesMemo = {};

class EventObserver {
  constructor(props) {
    // INIT - 4
    // $FlowFixMe
    this.mainWindow = props;
    // this.channel = {};
  }
  next = async (event) => {
    logger.info('[HW-DEBUG] Ledger NEXT');
    const transportList = await TransportNodeHid.list();
    const connectionChanged = event.type === 'add' || event.type === 'remove';
    if (connectionChanged) {
      logger.info('[HW-DEBUG] Ledger NEXT - connection changed');
      const device = get(event, 'device', {});
      const deviceModel = get(event, 'deviceModel', {});

      if (event.type === 'add') {
        if (!devicesMemo[device.path]) {
          logger.info('[HW-DEBUG] CONSTRUCTOR ADD');
          try {
            const transport = await TransportNodeHid.open(device.path);
            const AdaConnection = new AppAda(transport);
            devicesMemo[device.path] = {
              transport,
              AdaConnection: AdaConnection,
            };
            getHardwareWalletConnectionChannel.send(
              {
                disconnected: false,
                deviceType: 'ledger',
                deviceId: null, // Available only when Cardano APP opened
                deviceModel: deviceModel.id, // e.g. nanoS
                deviceName: deviceModel.productName, // e.g. Test Name
                path: device.path,
              },
              // $FlowFixMe
              this.mainWindow
            );
          } catch (e) {
            logger.info('[HW-DEBUG] CONSTRUCTOR error');
          }
        }
      } else {
        logger.info('[HW-DEBUG] CONSTRUCTOR REMOVE');
        devicesMemo = omit(devicesMemo, [device.path]);
        getHardwareWalletConnectionChannel.send(
          {
            disconnected: true,
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
      logger.info('[HW-DEBUG] CONSTRUCTOR Memo');

      // logger.info('[HW-DEBUG] SET NEW CHANNEL INIT');
      // if (event.type === 'add') {
      //   const transport = await TransportNodeHid.open(device.path);
      //   const AdaConnection = new AppAda(transport);
      //   logger.info('[HW-DEBUG] SET NEW CHANNEL: ', {transport, AdaConnection});
      //   this.channel = {
      //     ...this.channel,
      //     [device.path]: {
      //       transport,
      //       AdaConnection: AdaConnection,
      //     },
      //   }
      // }
    } else {
      logger.info('[HW-DEBUG] Ledger NEXT - connection NOT changed');
    }
  };
  error(e) {
    logger.info('[HW-DEBUG] Ledger NEXT error');
    throw e;
  }
  complete(event) {
    logger.info('[HW-DEBUG] Ledger NEXT complete');
  }
}

// INIT - 2
export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  logger.info('[HW-DEBUG] handleHardwareWalletDevices 1');
  const handleCheckHardwareWalletDevices = async () => {
    logger.info('[HW-DEBUG] handleCheckHardwareWalletDevices 1');
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
  let observer;

  getHardwareWalletTransportChannel.onRequest(async (request) => {
    logger.info('[HW-DEBUG] getHardwareWalletTransportChannel');
    // INIT - 6
    const { isTrezor, devicePath } = request;
    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: TREZOR ');
      try {
        deviceFeatures = await TrezorConnect.getFeatures({
          device: { path: devicePath },
        });
        logger.info('[HW-DEBUG] Trezor connect success');
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
        logger.info('[HW-DEBUG] Trezor connect error');
        throw e;
      }
    }

    // @TODO - uncomment once Ledger enabled
    try {
      //       // New Ledger TRY
      //       logger.info('[HW-DEBUG] TRY 3: ', { observer: observer.channel, devicePath });
      //
      //       const deviceTransport = observer.channel[devicePath].transport;
      //       logger.info('[HW-DEBUG] CHANNEL: ', deviceTransport);
      //
      //
      //       const { deviceModel } = deviceTransport;
      //       if (deviceModel) {
      //         logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case RESPONSE: ', deviceTransport);
      //         const info = await deviceTransport.device.getDeviceInfo();
      //         logger.info('[HW-DEBUG] !!! INFO !!!: ', info);
      //         const { id, productName } = deviceModel;
      //         return Promise.resolve({
      //           deviceId: null, // @TODO - to be defined
      //           deviceType: 'ledger',
      //           deviceModel: id, // e.g. nanoS
      //           deviceName: productName, // e.g. Ledger Nano S
      //           path: devicePath,
      //           firmwareVersion: null,
      //         });
      //       }
      //       return
      //       // END of try

      logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER');
      const transportList = await TransportNodeHid.list();
      let hw;
      let deviceConnection;
      let transport;

      // const path = devicePath || last(transportList);
      logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: PATH');
      let lastConnectedPath;

      // const noInstance = (devicePath && !devicesMemo[devicePath]) || (!devicePath && !transportList.length);

      if (transportList && !transportList.length) {
        // Establish connection with last device
        try {
          logger.info('[HW-DEBUG] INIT NEW transport');
          // hw = await TransportNodeHid.open(devicePath);
          logger.info('[HW-DEBUG] OPEN <<<');
          hw = await TransportNodeHid.create();
          logger.info('[HW-DEBUG] INIT NEW transport - DONE');
          const transportList = await TransportNodeHid.list();
          lastConnectedPath = last(transportList);
          logger.info('[HW-DEBUG] !!! lastConnectedPath !!!');

          deviceConnection = new AppAda(hw);
          devicesMemo[lastConnectedPath] = {
            transport: hw,
            AdaConnection: deviceConnection,
          };
        } catch (e) {
          logger.info('[HW-DEBUG] INIT NEW transport - ERROR');
          throw e;
        }
        // throw new Error('DEVICE_NOT_CONNECTED');
      } else if (!devicesMemo[devicePath]) {
        logger.info('[HW-DEBUG] USE First');
        // Use first like native usb nodeHID
        lastConnectedPath = transportList[0];
        // devicesMemo[lastConnectedPath]
        if (devicesMemo[lastConnectedPath]) {
          hw = devicesMemo[lastConnectedPath].transport;
          deviceConnection = devicesMemo[lastConnectedPath].AdaConnection;
        } else {
          throw new Error('Device not connected!');
        }
      } else {
        logger.info('[HW-DEBUG] USE CURRENT CONNECTION');
        hw = devicesMemo[devicePath].transport;
        deviceConnection = devicesMemo[devicePath].AdaConnection;
      }

      // No Device instance established at this point or disconnected flag set
      // if (
      //   !deviceConnection ||
      //   (deviceConnection &&
      //     deviceConnection.transport &&
      //     // $FlowFixMe
      //     deviceConnection.transport.disconnected)
      // ) {
      //   logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case 1');
      //   // Path exist so instance should be established for specific device
      //   if (path) {
      //     logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case 1.1');
      //     hw = await TransportNodeHid.open(path);
      //     // hw = await TransportNodeHid.create();
      //   } else {
      //     // Path NOT exist so instance should be established for last inserted device
      //     logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case 1.2');
      //     hw = await TransportNodeHid.create();
      //   }
      // } else {
      //   // Instance already exists for device with specific path
      //   logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case 2');
      //   hw = deviceConnection.transport;
      // }

      // if (!deviceConnection) {
      //   deviceConnection = new AppAda(hw);
      //   logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case get appAda: ', deviceConnection);
      // }

      const { deviceModel } = hw;
      if (deviceModel) {
        logger.info(
          '[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case RESPONSE'
        );
        const info = await hw.device.getDeviceInfo();
        logger.info('[HW-DEBUG] !!! INFO !!!');
        const { id, productName } = deviceModel;
        return Promise.resolve({
          deviceId: null, // @TODO - to be defined
          deviceType: 'ledger',
          deviceModel: id, // e.g. nanoS
          deviceName: productName, // e.g. Ledger Nano S
          path: lastConnectedPath || devicePath,
          firmwareVersion: null,
        });
      }
      throw new Error('Missing device info');
    } catch (error) {
      logger.info('[HW-DEBUG] ERROR on getHardwareWalletTransportChannel');
      throw error;
    }
  });

  handleInitTrezorConnectChannel.onRequest(async () => {
    logger.info('[HW-DEBUG] INIT TREZOR');
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
    logger.info('[HW-DEBUG] INIT LEDGER');
    observer = new EventObserver(mainWindow);
    try {
      logger.info('[HW-DEBUG] OBSERVER INIT');
      await TransportNodeHid.listen(observer);
      logger.info('[HW-DEBUG] OBSERVER INIT - listener started');
    } catch (e) {
      logger.info('[HW-DEBUG] OBSERVER INIT FAILED');
    }
  });

  deriveXpubChannel.onRequest(async (params) => {
    const { parentXpubHex, lastIndex, derivationScheme, test_new } = params;
    const parentXpub = utils.hex_to_buf(parentXpubHex);
    try {
      const xpub = deriveChildXpub(parentXpub, lastIndex, derivationScheme);
      return utils.buf_to_hex(xpub);
    } catch (e) {
      throw e;
    }
  });

  getCardanoAdaAppChannel.onRequest(async (request) => {
    const { path } = request;
    //    logger.info('[HW-DEBUG] TRY 3: ', { observer, path });

    //    let deviceConnection = observer.channel[path].AdaConnection;
    //    logger.info('[HW-DEBUG] CHANNEL: ', deviceConnection);

    //    try {
    //      const appVersion = await deviceConnection.getVersion();
    //      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion ', appVersion);
    //      const deviceSerial = await deviceConnection.getSerial();
    //      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
    //      const { minor, major, patch } = appVersion;
    //      return Promise.resolve({
    //        minor,
    //        major,
    //        patch,
    //        deviceId: deviceSerial.serial,
    //      });
    //    } catch (error) {
    //      logger.info('[HW-DEBUG] ERROR2 : ', error);
    //      if (error.name === 'DisconnectedDevice') {
    //        // Set old connection to null and force reinitialization once method called again
    //        // deviceConnection = null;
    //        /* await TransportNodeHid.open(path);
    //        logger.info('[HW-DEBUG] SET PERO');
    //        observer.channel[path] = 'PERO'; */
    //        // logger.info('[HW-DEBUG] SET NEW OBSERVER <<<');
    //        // observer = new EventObserver(mainWindow);
    //        // logger.info('[HW-DEBUG] OBSERVER INIT: ', observer)
    //        // const listener = await TransportNodeHid.listen(observer);
    //      }
    //      throw error;
    //    }

    //    return;

    //    if (!deviceConnection) {
    //      logger.info('[HW-DEBUG] NO DEVICE CONN: <<', { deviceConnection });
    //      try {
    //        const transportList = await TransportNodeHid.list();
    //        logger.info('[HW-DEBUG] transportList: ', transportList);

    //        logger.info('[HW-DEBUG] DOES PATH EXIST: ', path);
    //        const transport = await TransportNodeHid.open(transportList[0]);

    //        // let transport;
    //        // if (path) {
    //        //   transport = await TransportNodeHid.open(path);
    //        //   // logger.info('[HW-DEBUG] path: ', path);
    //        //   // const transport = await TransportNodeHid.open(path);
    //        // } else {
    //        //   logger.info('[HW-DEBUG] CREATE blocked');
    //        //   // transport = await TransportNodeHid.create();
    //        // }
    //        logger.info('[HW-DEBUG] transport: ', transport);
    //        deviceConnection = new AppAda(transport);
    //      } catch (e) {
    //        logger.info('[HW-DEBUG] ERROR 1: ', e);
    //        // deviceConnection = null;
    //        throw e;
    //      }
    //    }

    try {
      if (!devicesMemo[path]) {
        logger.info('[HW-DEBUG] Device not instantiated!');
        // [HW-DEBUG] USE FIRST maybe @TODO
        throw { code: 'DEVICE_NOT_CONNECTED' };
      }
      logger.info('[HW-DEBUG] GET CARDANO APP');
      deviceConnection = devicesMemo[path].AdaConnection;
      logger.info('[HW-DEBUG] GET CARDANO APP - deviceConnection');

      const appVersion = await deviceConnection.getVersion();
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion');
      const deviceSerial = await deviceConnection.getSerial();
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: deviceSerial');
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      logger.info('[HW-DEBUG] ERROR in Cardano App');
      if (error.name === 'DisconnectedDevice') {
        logger.info(
          '[HW-DEBUG] ERROR in Cardano App (CODE - DisconnectedDevice)'
        );
        // Set old connection to null and force reinitialization once method called again
        // deviceConnection = null;
        //   const transport = await TransportNodeHid.open(device.path);
        //   const AdaConnection = new AppAda(transport);
        const newTransport = await TransportNodeHid.open(path);
        deviceConnection = new AppAda(newTransport);
        // Update devicesMemo
        devicesMemo[path] = {
          transport: newTransport,
          AdaConnection: deviceConnection,
        };
      }
      throw error;
    }
    return;

    /*
    const transportList = await TransportNodeHid.list();
    logger.info('[HW-DEBUG] getCardanoAdaAppChannel: ', {transportList, deviceConnection});
    // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel
    if (!deviceConnection) {
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: NO device connection');
      try {
        // const newDeviceConnection = await TransportNodeHid.open(
        //   transportList[0]
        // );
        const newDeviceConnection = await TransportNodeHid.create();
        logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: newDeviceConnection: ', {newDeviceConnection});
        deviceConnection = new AppAda(newDeviceConnection);
        logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: newDeviceConnection - APP ADA instance: ', {deviceConnection});
      } catch (e) {
        logger.info('[HW-DEBUG] getCardanoAdaAppChannel::ERROR 1 ', e);
        throw e;
      }
    }
    logger.info('[HW-DEBUG] DC: ', deviceConnection);

    // deviceConnection = await TransportNodeHid.create()
    // logger.info('[HW-DEBUG] NWEW 2:', deviceConnection);

    // const aa = await TransportNodeHid.create()
    // deviceConnection = new AppAda(aa);

    try {
      const appVersion = await deviceConnection.getVersion();
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion ', appVersion);
      const deviceSerial = await deviceConnection.getSerial();
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel::ERROR 2 ', error);
      throw error;
    } */
  });

  getExtendedPublicKeyChannel.onRequest(async (params) => {
    // params example:
    // { path: "1852'/1815'/0'", isTrezor: false, devicePath: null }
    logger.info('[HW-DEBUG] getExtendedPublicKeyChannel');
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

      // New Ledger TRY
      // logger.info('[HW-DEBUG] TRY 4 - export keys: ', { observer: observer.channel, devicePath });
      //
      // const deviceTransport = observer.channel[devicePath].transport;
      // const deviceConnection = observer.channel[devicePath].AdaConnection;
      // logger.info('[HW-DEBUG] CHANNEL: ', deviceConnection);

      logger.info('[HW-DEBUG] EXPORT KEY');
      deviceConnection = devicesMemo[devicePath].AdaConnection;

      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }
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
      devicePath,
    } = params;

    logger.info('[HW-DEBUG] SIGN Ledger transaction');
    deviceConnection = devicesMemo[devicePath].AdaConnection;

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
