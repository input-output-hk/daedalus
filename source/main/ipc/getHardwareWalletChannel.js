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
import { logger } from '../utils/logging';
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

let devicesMemo = {};

class EventObserver {
  constructor(props) {
    // INIT - 4
    // $FlowFixMe
    this.mainWindow = props;
    this.test = null;
    this.channel = {};
  }
  next = async (event) => {
    const transportList = await TransportNodeHid.list();
    logger.info('>>> Ledger NEXT: ', { event, transportList });
    const connectionChanged = event.type === 'add' || event.type === 'remove';
    if (connectionChanged) {
      const device = get(event, 'device', {});
      const deviceModel = get(event, 'deviceModel', {});
      this.test = 100;

      if (event.type === 'add') {
        if (!devicesMemo[device.path]) {
          logger.info('>>> CONSTRUCTOR ADD: ', {
            device,
            transportList,
            devicesMemo,
          });
          try {
            const transport = await TransportNodeHid.open(device.path);
            logger.info('>>> TRANSPORT: ', { transport });
            const AdaConnection = new AppAda(transport);
            logger.info('>>> ADA CONNECTION: ', { AdaConnection });
            devicesMemo[device.path] = {
              transport,
              AdaConnection: AdaConnection,
            };
            logger.info('>>> getHardwareWalletConnectionChannel 1');
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
            logger.info('>>> getHardwareWalletConnectionChannel 2');
          } catch (e) {
            logger.info('>>> CONSTRUCTOR error: ', e);
          }
        }
      } else {
        logger.info('>>> CONSTRUCTOR REMOVE: ', device);
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
      logger.info('>>> CONSTRUCTOR Memo: ', { devicesMemo });

      // logger.info('>>> SET NEW CHANNEL INIT');
      // if (event.type === 'add') {
      //   const transport = await TransportNodeHid.open(device.path);
      //   const AdaConnection = new AppAda(transport);
      //   logger.info('>>> SET NEW CHANNEL: ', {transport, AdaConnection});
      //   this.channel = {
      //     ...this.channel,
      //     [device.path]: {
      //       transport,
      //       AdaConnection: AdaConnection,
      //     },
      //   }
      // }
    }
  };
  error(e) {
    logger.info('>>> Ledger NEXT error: ', e);
    throw e;
  }
  complete(event) {
    logger.info('>>> Ledger NEXT complete: ', event);
  }
}

// INIT - 2
export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  logger.info('>>> handleHardwareWalletDevices 1');
  const handleCheckHardwareWalletDevices = async () => {
    logger.info('>>> handleCheckHardwareWalletDevices 1');
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
  logger.info('>>> handleHardwareWalletRequests');
  let deviceConnection = null;
  let observer;

  getHardwareWalletTransportChannel.onRequest(async (request) => {
    logger.info('>>> getHardwareWalletTransportChannel: ', request);
    // INIT - 6
    const { isTrezor, devicePath } = request;
    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      logger.info('>>> getHardwareWalletTransportChannel:: TREZOR ');
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
      //       // New Ledger TRY
      //       logger.info('>>> TRY 3: ', { observer: observer.channel, devicePath });
      //
      //       const deviceTransport = observer.channel[devicePath].transport;
      //       logger.info('>>> CHANNEL: ', deviceTransport);
      //
      //
      //       const { deviceModel } = deviceTransport;
      //       if (deviceModel) {
      //         logger.info('>>> getHardwareWalletTransportChannel:: LEDGER case RESPONSE: ', deviceTransport);
      //         const info = await deviceTransport.device.getDeviceInfo();
      //         logger.info('>>> !!! INFO !!!: ', info);
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

      logger.info('>>> getHardwareWalletTransportChannel:: LEDGER: ', {
        devicesMemo,
      });
      const transportList = await TransportNodeHid.list();
      let hw;
      let deviceConnection;
      let transport;

      // const path = devicePath || last(transportList);
      logger.info('>>> getHardwareWalletTransportChannel:: PATH: ', {
        devicePath,
        transportList,
      });
      let lastConnectedPath;

      // const noInstance = (devicePath && !devicesMemo[devicePath]) || (!devicePath && !transportList.length);

      if (transportList && !transportList.length) {
        // Establish connection with last device
        try {
          logger.info('>>> INIT NEW transport');
          // hw = await TransportNodeHid.open(devicePath);
          logger.info('>>> OPEN <<<: ', devicePath);
          hw = await TransportNodeHid.create();
          logger.info('>>> INIT NEW transport - DONE: ', hw);
          const transportList = await TransportNodeHid.list();
          lastConnectedPath = last(transportList);
          logger.info('>>> !!! lastConnectedPath !!!: ', lastConnectedPath);

          deviceConnection = new AppAda(hw);
          devicesMemo[lastConnectedPath] = {
            transport: hw,
            AdaConnection: deviceConnection,
          };
        } catch (e) {
          logger.info('>>> INIT NEW transport - ERROR: ', e);
          throw e;
        }
        // throw new Error('DEVICE_NOT_CONNECTED');
      } else if (!devicesMemo[devicePath]) {
        logger.info('>>> USE First ');
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
        logger.info('>>> USE CURRENT CONNECTION');
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
      //   logger.info('>>> getHardwareWalletTransportChannel:: LEDGER case 1');
      //   // Path exist so instance should be established for specific device
      //   if (path) {
      //     logger.info('>>> getHardwareWalletTransportChannel:: LEDGER case 1.1');
      //     hw = await TransportNodeHid.open(path);
      //     // hw = await TransportNodeHid.create();
      //   } else {
      //     // Path NOT exist so instance should be established for last inserted device
      //     logger.info('>>> getHardwareWalletTransportChannel:: LEDGER case 1.2');
      //     hw = await TransportNodeHid.create();
      //   }
      // } else {
      //   // Instance already exists for device with specific path
      //   logger.info('>>> getHardwareWalletTransportChannel:: LEDGER case 2');
      //   hw = deviceConnection.transport;
      // }

      // if (!deviceConnection) {
      //   deviceConnection = new AppAda(hw);
      //   logger.info('>>> getHardwareWalletTransportChannel:: LEDGER case get appAda: ', deviceConnection);
      // }

      const { deviceModel } = hw;
      if (deviceModel) {
        logger.info(
          '>>> getHardwareWalletTransportChannel:: LEDGER case RESPONSE: ',
          hw
        );
        const info = await hw.device.getDeviceInfo();
        logger.info('>>> !!! INFO !!!: ', info);
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
      logger.info('>>> ERROR on getHardwareWalletTransportChannel: ', {
        error,
        deviceConnection,
      });
      throw error;
    }
  });

  handleInitTrezorConnectChannel.onRequest(async () => {
    logger.info('>>> INIT TREZOR <<<');
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
    logger.info('>>> INIT LEDGER <<<');
    observer = new EventObserver(mainWindow);
    logger.info('>>> OBSERVER INIT: ', observer);
    const listener = await TransportNodeHid.listen(observer);
    logger.info('>>> LISTERNER: ', listener);
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
    //    logger.info('>>> TRY 3: ', { observer, path });

    //    let deviceConnection = observer.channel[path].AdaConnection;
    //    logger.info('>>> CHANNEL: ', deviceConnection);

    //    try {
    //      const appVersion = await deviceConnection.getVersion();
    //      logger.info('>>> getCardanoAdaAppChannel:: appVersion ', appVersion);
    //      const deviceSerial = await deviceConnection.getSerial();
    //      logger.info('>>> getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
    //      const { minor, major, patch } = appVersion;
    //      return Promise.resolve({
    //        minor,
    //        major,
    //        patch,
    //        deviceId: deviceSerial.serial,
    //      });
    //    } catch (error) {
    //      logger.info('>>> ERROR2 : ', error);
    //      if (error.name === 'DisconnectedDevice') {
    //        // Set old connection to null and force reinitialization once method called again
    //        // deviceConnection = null;
    //        /* await TransportNodeHid.open(path);
    //        logger.info('>>> SET PERO');
    //        observer.channel[path] = 'PERO'; */
    //        // logger.info('>>> SET NEW OBSERVER <<<');
    //        // observer = new EventObserver(mainWindow);
    //        // logger.info('>>> OBSERVER INIT: ', observer)
    //        // const listener = await TransportNodeHid.listen(observer);
    //      }
    //      throw error;
    //    }

    //    // Some new test

    //    // END of test
    //    return;
    if (!devicesMemo[path]) {
      logger.info('>>> Device not instantiated!');
      // >>> USE FIRST @TODO
      throw new Error('Device not connected!');
    }
    logger.info('>>> GET CARDANO APP: ', { devicesMemo, path });
    deviceConnection = devicesMemo[path].AdaConnection;
    logger.info('>>> GET CARDANO APP - deviceConnection: ', {
      deviceConnection,
    });

    //    if (!deviceConnection) {
    //      logger.info('>>> NO DEVICE CONN: <<', { deviceConnection });
    //      try {
    //        const transportList = await TransportNodeHid.list();
    //        logger.info('>>> transportList: ', transportList);

    //        logger.info('>>> DOES PATH EXIST: ', path);
    //        const transport = await TransportNodeHid.open(transportList[0]);

    //        // let transport;
    //        // if (path) {
    //        //   transport = await TransportNodeHid.open(path);
    //        //   // logger.info('>>> path: ', path);
    //        //   // const transport = await TransportNodeHid.open(path);
    //        // } else {
    //        //   logger.info('>>> CREATE blocked');
    //        //   // transport = await TransportNodeHid.create();
    //        // }
    //        logger.info('>>> transport: ', transport);
    //        deviceConnection = new AppAda(transport);
    //      } catch (e) {
    //        logger.info('>>> ERROR 1: ', e);
    //        // deviceConnection = null;
    //        throw e;
    //      }
    //    }

    logger.info('>>>> deviceConnection: ', {
      _____deviceConnection: deviceConnection,
      _____TransportNodeHid: TransportNodeHid,
    });

    try {
      const appVersion = await deviceConnection.getVersion();
      logger.info('>>> getCardanoAdaAppChannel:: appVersion ', appVersion);
      const deviceSerial = await deviceConnection.getSerial();
      logger.info('>>> getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      logger.info('>>> ERROR2 : ', error);
      if (error.name === 'DisconnectedDevice') {
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

    const transportList = await TransportNodeHid.list();
    logger.info('>>> getCardanoAdaAppChannel: ', {
      transportList,
      deviceConnection,
    });
    // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel
    if (!deviceConnection) {
      logger.info('>>> getCardanoAdaAppChannel:: NO device connection');
      try {
        // const newDeviceConnection = await TransportNodeHid.open(
        //   transportList[0]
        // );
        const newDeviceConnection = await TransportNodeHid.create();
        logger.info('>>> getCardanoAdaAppChannel:: newDeviceConnection: ', {
          newDeviceConnection,
        });
        deviceConnection = new AppAda(newDeviceConnection);
        logger.info(
          '>>> getCardanoAdaAppChannel:: newDeviceConnection - APP ADA instance: ',
          { deviceConnection }
        );
      } catch (e) {
        logger.info('>>> getCardanoAdaAppChannel::ERROR 1 ', e);
        throw e;
      }
    }
    logger.info('>>> DC: ', deviceConnection);

    // deviceConnection = await TransportNodeHid.create()
    // logger.info('>>> NWEW 2:', deviceConnection);

    // const aa = await TransportNodeHid.create()
    // deviceConnection = new AppAda(aa);

    try {
      const appVersion = await deviceConnection.getVersion();
      logger.info('>>> getCardanoAdaAppChannel:: appVersion ', appVersion);
      const deviceSerial = await deviceConnection.getSerial();
      logger.info('>>> getCardanoAdaAppChannel:: deviceSerial ', deviceSerial);
      const { minor, major, patch } = appVersion;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: deviceSerial.serial,
      });
    } catch (error) {
      logger.info('>>> getCardanoAdaAppChannel::ERROR 2 ', error);
      throw error;
    }
  });

  getExtendedPublicKeyChannel.onRequest(async (params) => {
    // params example:
    // { path: "1852'/1815'/0'", isTrezor: false, devicePath: null }
    logger.info('>>> getExtendedPublicKeyChannel: ', params);
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
      // logger.info('>>> TRY 4 - export keys: ', { observer: observer.channel, devicePath });
      //
      // const deviceTransport = observer.channel[devicePath].transport;
      // const deviceConnection = observer.channel[devicePath].AdaConnection;
      // logger.info('>>> CHANNEL: ', deviceConnection);

      logger.info('>>> EXPORT KEY: ', { devicesMemo, path, devicePath });
      deviceConnection = devicesMemo[devicePath].AdaConnection;
      logger.info('>>> EXPORT KEY - deviceConnection: ', { deviceConnection });

      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }
      logger.info('>>> PATH: ', { path, toPath: cardano.str_to_path(path) });
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

    logger.info('>>> SIGN: ', { devicesMemo, devicePath });
    deviceConnection = devicesMemo[devicePath].AdaConnection;
    logger.info('>>> SIGN - deviceConnection: ', { deviceConnection });

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
