'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.handleHardwareWalletRequests = exports.ledgerStatus = void 0;
const hw_transport_node_hid_noevents_1 = __importStar(
  require('@ledgerhq/hw-transport-node-hid-noevents')
);
const ledgerjs_hw_app_cardano_1 = __importStar(
  require('@cardano-foundation/ledgerjs-hw-app-cardano')
);
const address_1 = require('@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address');
const connect_1 = __importStar(require('@trezor/connect'));
const lodash_1 = require('lodash');
const cardano_crypto_js_1 = require('cardano-crypto.js');
const deviceDetection_1 = require('./hardwareWallets/ledger/deviceDetection');
const logging_1 = require('../utils/logging');
const connection_1 = require('../trezor/connection');
exports.ledgerStatus = {
  listening: false,
  Listener: null,
};
let devicesMemo = {};
class EventObserver {
  mainWindow;
  getHardwareWalletConnectionChannel;
  constructor({ mainWindow, getHardwareWalletConnectionChannel }) {
    this.mainWindow = mainWindow;
    this.getHardwareWalletConnectionChannel = getHardwareWalletConnectionChannel;
  }
  next = async (event) => {
    try {
      const transportList = await hw_transport_node_hid_noevents_1.default.list();
      const connectionChanged = event.type === 'add' || event.type === 'remove';
      logging_1.logger.info(
        `[HW-DEBUG] Ledger NEXT: , ${JSON.stringify({
          event,
          transportList,
          connectionChanged,
        })}`
      );
      if (connectionChanged) {
        logging_1.logger.info('[HW-DEBUG] Ledger NEXT - connection changed');
        const { device, deviceModel } = event;
        if (event.type === 'add') {
          if (!devicesMemo[device.path]) {
            logging_1.logger.info('[HW-DEBUG] CONSTRUCTOR ADD');
            const walletData = {
              disconnected: false,
              deviceType: 'ledger',
              deviceId: null,
              // Available only when Cardano APP opened
              deviceModel: deviceModel.id,
              // e.g. nanoS
              deviceName: deviceModel.productName,
              // e.g. Test Name
              path: device.path,
              product: device.product,
            };
            try {
              const transport = await hw_transport_node_hid_noevents_1.default.open(
                device.path
              );
              const AdaConnection = new ledgerjs_hw_app_cardano_1.default(
                transport
              );
              devicesMemo[device.path] = {
                device,
                transport,
                AdaConnection,
              };
              this.getHardwareWalletConnectionChannel.send(
                walletData,
                this.mainWindow
              );
            } catch (error) {
              logging_1.logger.error('[HW-DEBUG] CONSTRUCTOR error', {
                walletData,
                error,
              });
            }
          }
        } else {
          logging_1.logger.info('[HW-DEBUG] CONSTRUCTOR REMOVE');
          devicesMemo = (0, lodash_1.omit)(devicesMemo, [device.path]);
          this.getHardwareWalletConnectionChannel.send(
            {
              disconnected: true,
              deviceType: 'ledger',
              deviceId: null,
              // Available only when Cardano APP opened
              deviceModel: deviceModel.id,
              // e.g. nanoS
              deviceName: deviceModel.productName,
              // e.g. Test Name
              path: device.path,
              product: device.product,
            },
            this.mainWindow
          );
        }
        logging_1.logger.info('[HW-DEBUG] CONSTRUCTOR Memo');
      } else {
        logging_1.logger.info(
          '[HW-DEBUG] Ledger NEXT - connection NOT changed'
        );
      }
    } catch (error) {
      logging_1.logger.error(
        `[HW-DEBUG] Error on NEXT ${JSON.stringify(error)}`
      );
    }
  };
  error(e) {
    logging_1.logger.info('[HW-DEBUG] Ledger NEXT error');
    throw e;
  }
  complete() {
    logging_1.logger.info('[HW-DEBUG] Ledger NEXT complete');
  }
}
const handleHardwareWalletRequests = async (
  mainWindow,
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
  }
) => {
  let deviceConnection = null;
  let observer;
  const resetTrezorListeners = () => {
    // Remove all listeners if exist - e.g. on app refresh
    connect_1.default.removeAllListeners();
    // Initialize new device listeners
    connect_1.default.on(connect_1.UI_EVENT, (event) => {
      logging_1.logger.info(
        `[TREZOR-CONNECT] Received UI_EVENT: ${event.type}`
      );
      if (event.type === connect_1.UI.REQUEST_PASSPHRASE) {
        // ui-request_passphrase
        if (event.payload && event.payload.device) {
          connect_1.default.uiResponse({
            type: connect_1.UI.RECEIVE_PASSPHRASE,
            payload: {
              save: true,
              value: '',
              passphraseOnDevice: true,
            },
          });
          logging_1.logger.info(
            '[TREZOR-CONNECT] Called TrezorConnect.uiResponse - requested to provide passphrase on device'
          );
        }
      }
    });
    connect_1.default.on(connect_1.TRANSPORT_EVENT, (event) => {
      logging_1.logger.info(
        '[TREZOR-CONNECT] Received TRANSPORT_EVENT: transport-error',
        event.payload
      );
      if (event.type === connect_1.TRANSPORT.ERROR) {
        // Send Transport error to Renderer
        getHardwareWalletConnectionChannel.send(
          {
            deviceType: 'trezor',
            error: {
              payload: event.payload,
            },
          },
          // @ts-ignore
          mainWindow
        );
      }
    });
    connect_1.default.on(connect_1.DEVICE_EVENT, (event) => {
      logging_1.logger.info(
        `[TREZOR-CONNECT] Received DEVICE_EVENT: ${event.type}`
      );
      const connectionChanged =
        event.type === connect_1.DEVICE.CONNECT ||
        event.type === connect_1.DEVICE.DISCONNECT ||
        event.type === connect_1.DEVICE.CHANGED;
      const isAcquired =
        (0, lodash_1.get)(event, ['payload', 'type'], '') === 'acquired';
      const deviceError = (0, lodash_1.get)(event, ['payload', 'error']);
      if (deviceError) {
        throw new Error(deviceError);
      }
      if (connectionChanged && isAcquired) {
        getHardwareWalletConnectionChannel.send(
          {
            disconnected: event.type === connect_1.DEVICE.DISCONNECT,
            deviceType: 'trezor',
            deviceId: event.payload.id,
            // 123456ABCDEF
            deviceModel: event.payload.features.model,
            // e.g. T
            deviceName: event.payload.label,
            // e.g. Test Name
            path: event.payload.path,
            eventType: event.type,
          },
          // @ts-ignore
          mainWindow
        );
      }
    });
  };
  waitForLedgerDevicesToConnectChannel.onRequest(async () => {
    logging_1.logger.info(
      '[HW-DEBUG] waitForLedgerDevicesToConnectChannel::waiting'
    );
    const { device, deviceModel } = await (0,
    deviceDetection_1.waitForDevice)();
    logging_1.logger.info(
      '[HW-DEBUG] waitForLedgerDevicesToConnectChannel::found'
    );
    return {
      disconnected: false,
      deviceType: 'ledger',
      deviceId: null,
      // Available only when Cardano APP opened
      deviceModel: deviceModel.id,
      // e.g. nanoS
      deviceName: deviceModel.productName,
      // e.g. Test Name
      path: device.path,
      product: device.product,
    };
  });
  getHardwareWalletTransportChannel.onRequest(async (request) => {
    const { isTrezor, devicePath } = request;
    logging_1.logger.info('[HW-DEBUG] getHardwareWalletTransportChannel', {
      devicePath,
    });
    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      logging_1.logger.info(
        '[HW-DEBUG] getHardwareWalletTransportChannel::TREZOR '
      );
      try {
        deviceFeatures = await connect_1.default.getFeatures({
          device: {
            path: devicePath,
          },
        });
        logging_1.logger.info(
          '[TREZOR-CONNECT] Called TrezorConnect.getFeatures()'
        );
        if (deviceFeatures && deviceFeatures.success) {
          logging_1.logger.info('[HW-DEBUG] Trezor connect success');
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
            deviceModel: model,
            // e.g. "1" or "T"
            deviceName: label,
            path: devicePath,
            firmwareVersion,
          });
        }
        throw deviceFeatures.payload; // Error is in payload
      } catch (e) {
        logging_1.logger.info(
          '[HW-DEBUG] Trezor connect error: ',
          e.message || 'no message'
        );
        throw e;
      }
    }
    try {
      logging_1.logger.info(
        '[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER'
      );
      const transportList = await hw_transport_node_hid_noevents_1.default.list();
      let hw;
      let lastConnectedPath;
      logging_1.logger.info(
        `[HW-DEBUG] getHardwareWalletTransportChannel::transportList=${JSON.stringify(
          transportList
        )}`
      );
      const openTransportLayer = async (pathToOpen, device) => {
        if (devicesMemo[pathToOpen]) {
          logging_1.logger.info('[HW-DEBUG] CLOSING EXISTING TRANSPORT');
          await devicesMemo[pathToOpen].transport.close();
        }
        const transport = await hw_transport_node_hid_noevents_1.default.open(
          pathToOpen
        );
        hw = transport;
        lastConnectedPath = pathToOpen;
        logging_1.logger.info('[HW-DEBUG] INIT NEW transport - DONE');
        deviceConnection = new ledgerjs_hw_app_cardano_1.default(transport);
        devicesMemo[lastConnectedPath] = {
          device,
          transport: hw,
          AdaConnection: deviceConnection,
        };
      };
      if (transportList && !transportList.length) {
        // Establish connection with last device
        try {
          logging_1.logger.info('[HW-DEBUG] INIT NEW transport');
          const { device } = await (0, deviceDetection_1.waitForDevice)();
          await openTransportLayer(device.path, device);
        } catch (e) {
          logging_1.logger.info('[HW-DEBUG] INIT NEW transport - ERROR');
          throw e;
        }
      } else if (!devicePath || !devicesMemo[devicePath]) {
        // Use first like native usb nodeHID
        lastConnectedPath = transportList[0]; // eslint-disable-line
        logging_1.logger.info('[HW-DEBUG] USE First transport', {
          lastConnectedPath,
        });
        if (devicesMemo[lastConnectedPath]) {
          await openTransportLayer(
            lastConnectedPath,
            devicesMemo[lastConnectedPath].device
          );
        } else {
          throw new Error('Device not connected!');
        }
      } else {
        logging_1.logger.info('[HW-DEBUG] USE CURRENT CONNECTION');
        hw = devicesMemo[devicePath].transport;
        deviceConnection = (0, lodash_1.get)(devicesMemo, [
          devicePath,
          'AdaConnection',
        ]);
      }
      const { deviceModel } = hw;
      if (deviceModel) {
        const { id, productName } = deviceModel;
        const ledgerData = {
          deviceId: null,
          // @TODO - to be defined
          deviceType: 'ledger',
          deviceModel: id,
          // e.g. nanoS
          deviceName: productName,
          // e.g. Ledger Nano S
          path: lastConnectedPath || devicePath,
        };
        logging_1.logger.info(
          '[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case RESPONSE',
          { ledgerData }
        );
        return Promise.resolve(ledgerData);
      }
      throw new Error('Missing device info');
    } catch (error) {
      logging_1.logger.info(
        '[HW-DEBUG] ERROR on getHardwareWalletTransportChannel'
      );
      throw error;
    }
  });
  handleInitTrezorConnectChannel.onRequest(async () => {
    logging_1.logger.info('[HW-DEBUG] INIT TREZOR');
    await (0, connection_1.initTrezorConnect)();
    resetTrezorListeners();
  });
  handleInitLedgerConnectChannel.onRequest(async () => {
    logging_1.logger.info('[HW-DEBUG] INIT LEDGER');
    observer = new EventObserver({
      mainWindow: mainWindow,
      getHardwareWalletConnectionChannel,
    });
    try {
      logging_1.logger.info('[HW-DEBUG] OBSERVER INIT');
      const onAdd = (payload) => {
        observer.next(payload);
      };
      const onRemove = (payload) => {
        observer.next(payload);
      };
      (0, deviceDetection_1.deviceDetection)(onAdd, onRemove);
      logging_1.logger.info('[HW-DEBUG] OBSERVER INIT - listener started');
    } catch (e) {
      logging_1.logger.info('[HW-DEBUG] OBSERVER INIT FAILED');
      exports.ledgerStatus.listening = false;
    }
  });
  deriveXpubChannel.onRequest(async (params) => {
    const { parentXpubHex, lastIndex, derivationScheme } = params;
    const parentXpub = ledgerjs_hw_app_cardano_1.utils.hex_to_buf(
      parentXpubHex
    );
    try {
      const xpub = (0, cardano_crypto_js_1.derivePublic)(
        parentXpub,
        lastIndex,
        derivationScheme
      );
      return ledgerjs_hw_app_cardano_1.utils.buf_to_hex(xpub);
    } catch (e) {
      throw e;
    }
  });
  deriveAddressChannel.onRequest(async (params) => {
    const {
      addressType,
      spendingPathStr,
      stakingPathStr,
      devicePath,
      isTrezor,
      networkId,
      protocolMagic,
    } = params;
    const spendingPath = (0, address_1.str_to_path)(spendingPathStr);
    const stakingPath = stakingPathStr
      ? (0, address_1.str_to_path)(stakingPathStr)
      : null;
    deviceConnection = (0, lodash_1.get)(devicesMemo, [
      devicePath,
      'AdaConnection',
    ]);
    logging_1.logger.info('[HW-DEBUG] DERIVE ADDRESS');
    if (isTrezor) {
      logging_1.logger.info(
        '[TREZOR-CONNECT] Called TrezorConnect.cardanoGetAddress()'
      );
      const result = await connect_1.default.cardanoGetAddress({
        showOnTrezor: true,
        addressParameters: {
          addressType,
          path: `m/${spendingPathStr}`,
          stakingPath: stakingPathStr ? `m/${stakingPathStr}` : null,
        },
        protocolMagic,
        networkId,
      });
      if (result.success === false) {
        logging_1.logger.error(
          '[TREZOR-CONNECT] TrezorConnect.cardanoGetAddress() failed',
          result.payload
        );
        throw new Error('TrezorConnect.cardanoGetAddress() failed');
      }
      return result.payload.address;
    }
    // Check if Ledger instantiated
    if (!deviceConnection) {
      throw new Error('Ledger device not connected');
    }
    const { addressHex } = await deviceConnection.deriveAddress({
      network: {
        networkId,
        protocolMagic,
      },
      address: {
        type: addressType,
        params: {
          spendingPath,
          stakingPath,
        },
      },
    });
    const encodedAddress = ledgerjs_hw_app_cardano_1.utils.bech32_encodeAddress(
      ledgerjs_hw_app_cardano_1.utils.hex_to_buf(addressHex)
    );
    return encodedAddress;
  });
  showAddressChannel.onRequest(async (params) => {
    const {
      addressType,
      spendingPathStr,
      stakingPathStr,
      devicePath,
      isTrezor,
      networkId,
      protocolMagic,
    } = params;
    const spendingPath = (0, address_1.str_to_path)(spendingPathStr);
    const stakingPath = stakingPathStr
      ? (0, address_1.str_to_path)(stakingPathStr)
      : null;
    try {
      deviceConnection = (0, lodash_1.get)(devicesMemo, [
        devicePath,
        'AdaConnection',
      ]);
      logging_1.logger.info('[HW-DEBUG] SHOW ADDRESS');
      if (isTrezor) {
        throw new Error('Address verification not supported on Trezor devices');
      }
      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }
      await deviceConnection.showAddress({
        network: {
          networkId,
          protocolMagic,
        },
        address: {
          type: addressType,
          params: {
            spendingPath,
            stakingPath,
          },
        },
      });
      return;
    } catch (e) {
      throw e;
    }
  });
  getCardanoAdaAppChannel.onRequest(async (request) => {
    const { path, product } = request;
    try {
      if (!devicesMemo[path]) {
        const deviceList = (0, hw_transport_node_hid_noevents_1.getDevices)();
        const device =
          (0, lodash_1.find)(deviceList, ['product', product]) ||
          (0, lodash_1.find)(deviceList, ['path', path]);
        logging_1.logger.info(
          '[HW-DEBUG] getCardanoAdaAppChannel:: Path not found',
          {
            product,
            deviceList,
            oldPath: path,
          }
        );
        if (!device) {
          logging_1.logger.info('[HW-DEBUG] Device not instantiated!', {
            path,
            devicesMemo,
          });
          // eslint-disable-next-line
          throw {
            code: 'DEVICE_NOT_CONNECTED',
          };
        }
        const newTransport = await hw_transport_node_hid_noevents_1.default.open(
          device.path
        );
        const newDeviceConnection = new ledgerjs_hw_app_cardano_1.default(
          newTransport
        );
        logging_1.logger.info(
          '[HW-DEBUG] getCardanoAdaAppChannel::Use new device path',
          {
            product,
            device,
            newPath: device.path,
            oldPath: path,
          }
        );
        devicesMemo[device.path] = {
          device,
          transport: newTransport,
          AdaConnection: newDeviceConnection,
        };
        if (device.path !== path) {
          // eslint-disable-next-line
          throw {
            code: 'DEVICE_PATH_CHANGED',
            path: device.path,
          };
        }
      }
      if (!path || !devicesMemo[path]) {
        logging_1.logger.info('[HW-DEBUG] Device not instantiated!', {
          path,
          devicesMemo,
        });
        // eslint-disable-next-line
        throw {
          code: 'DEVICE_NOT_CONNECTED',
        };
      }
      logging_1.logger.info(`[HW-DEBUG] GET CARDANO APP path:${path}`);
      deviceConnection = devicesMemo[path].AdaConnection;
      const { version } = await deviceConnection.getVersion();
      logging_1.logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion');
      const { serialHex } = await deviceConnection.getSerial();
      logging_1.logger.info(
        `[HW-DEBUG] getCardanoAdaAppChannel:: deviceSerial: ${serialHex}`
      );
      const { minor, major, patch } = version;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: serialHex,
      });
    } catch (error) {
      const errorCode = error.code || '';
      const errorName = error.name || 'UknownErrorName';
      const errorMessage = error.message || 'UknownErrorMessage';
      const isDeviceDisconnected = errorCode === 'DEVICE_NOT_CONNECTED';
      const isDisconnectError =
        errorName === 'DisconnectedDevice' ||
        errorMessage === 'Cannot write to hid device';
      //  errorMessage.toLowerCase().includes('cannot open device with path') ||
      //  errorMessage.toLowerCase().includes('cannot write to hid device') ||
      //  errorMessage.toLowerCase().includes('cannot write to closed device');
      logging_1.logger.info('[HW-DEBUG] ERROR in Cardano App', {
        path,
        errorName,
        errorMessage,
        isDisconnectError,
        isDeviceDisconnected,
      });
      if (path && !isDeviceDisconnected && isDisconnectError) {
        const oldPath = path;
        const deviceMemo = devicesMemo[oldPath];
        const devicePaths = await hw_transport_node_hid_noevents_1.default.list();
        const hasPathChanged = !(0, lodash_1.includes)(devicePaths, oldPath);
        const newPath = hasPathChanged
          ? (0, lodash_1.last)(devicePaths)
          : oldPath;
        if (hasPathChanged) {
          logging_1.logger.info(
            `[HW-DEBUG] Device path changed from ${oldPath} to ${newPath}`
          );
        }
        if (!newPath) {
          logging_1.logger.info(
            '[HW-DEBUG] ERROR in Cardano App (Device paths list is empty)',
            {
              devicePaths,
              oldPath,
              newPath,
              deviceList: (0, hw_transport_node_hid_noevents_1.getDevices)(),
            }
          );
          // eslint-disable-next-line
          throw {
            code: 'NO_DEVICE_PATHS',
            errorCode,
            errorName,
          };
        }
        const { device: oldDevice } = deviceMemo;
        const newTransport = await hw_transport_node_hid_noevents_1.default.open(
          newPath
        );
        const newDeviceConnection = new ledgerjs_hw_app_cardano_1.default(
          newTransport
        );
        const deviceList = (0, hw_transport_node_hid_noevents_1.getDevices)();
        const newDevice = (0, lodash_1.find)(deviceList, ['path', newPath]);
        const hasDeviceChanged = newDevice.productId !== oldDevice.productId;
        // TODO: remove
        deviceConnection = newDeviceConnection;
        // Purge old device memo
        devicesMemo = (0, lodash_1.omit)(devicesMemo, [oldPath]);
        logging_1.logger.info(
          '[HW-DEBUG] ERROR in Cardano App (Re-establish Connection)',
          {
            hasPathChanged,
            hasDeviceChanged,
            oldPath: oldPath || 'UNKNOWN_PATH',
            newPath: newPath || 'UNKNOWN_PATH',
            oldDevice: oldDevice || 'NOT_FOUND',
            newDevice: newDevice || 'NOT_FOUND',
          }
        );
        // Update devicesMemo
        devicesMemo[newPath] = {
          device: newDevice,
          transport: newTransport,
          AdaConnection: newDeviceConnection,
        };
        if (hasPathChanged) {
          // eslint-disable-next-line
          throw {
            code: 'DEVICE_PATH_CHANGED',
            path: newPath,
          };
        }
      }
      throw error;
    }
  });
  getExtendedPublicKeyChannel.onRequest(async (params) => {
    // Params example:
    // { path: "1852'/1815'/0'", isTrezor: false, devicePath: null }
    logging_1.logger.info('[HW-DEBUG] getExtendedPublicKeyChannel');
    const { path, isTrezor, devicePath } = params;
    try {
      logging_1.logger.info(`[TREZOR-CONNECT] isTrezor=${isTrezor}`);
      if (isTrezor) {
        // We re-initialize the Trezor Connect session to give the user the chance to provide
        // a different passphrase, in case they want to switch to a different
        // hidden wallet or just if they provided a wrong one.
        // await reinitTrezorConnect();
        // resetTrezorListeners();
        logging_1.logger.info(
          '[TREZOR-CONNECT] Calling TrezorConnect.getFeatures()'
        );
        const deviceFeatures = await connect_1.default.getFeatures();
        if (deviceFeatures.success) {
          logging_1.logger.info(
            '[TREZOR-CONNECT] Calling TrezorConnect.cardanoGetPublicKey()'
          );
          const extendedPublicKeyResponse = await connect_1.default.cardanoGetPublicKey(
            {
              path: `m/${path}`,
              showOnTrezor: true,
            }
          );
          if (!extendedPublicKeyResponse.success) {
            throw extendedPublicKeyResponse.payload;
          }
          const extendedPublicKey = (0, lodash_1.get)(
            extendedPublicKeyResponse,
            ['payload', 'node']
          );
          return Promise.resolve({
            publicKeyHex: extendedPublicKey.public_key,
            chainCodeHex: extendedPublicKey.chain_code,
          });
        }
        throw new Error('Trezor device not connected');
      }
      deviceConnection = (0, lodash_1.get)(devicesMemo, [
        devicePath,
        'AdaConnection',
      ]);
      logging_1.logger.info('[HW-DEBUG] EXPORT KEY');
      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }
      const extendedPublicKey = await deviceConnection.getExtendedPublicKey({
        path: (0, address_1.str_to_path)(path),
      });
      const deviceSerial = await deviceConnection.getSerial();
      return Promise.resolve({
        publicKeyHex: extendedPublicKey.publicKeyHex,
        chainCodeHex: extendedPublicKey.chainCodeHex,
        deviceId: deviceSerial.serialHex,
      });
    } catch (error) {
      logging_1.logger.info('[HW-DEBUG] EXPORT KEY ERROR', error);
      throw error;
    }
  });
  // @TODO - validityIntervalStart is not working with Cardano App 2.1.0
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
      auxiliaryData,
      devicePath,
      signingMode,
      additionalWitnessPaths,
    } = params;
    logging_1.logger.info('[HW-DEBUG] SIGN Ledger transaction');
    deviceConnection = devicePath
      ? devicesMemo[devicePath].AdaConnection
      : null;
    try {
      if (!deviceConnection) {
        throw new Error('Device not connected!');
      }
      const signedTransaction = await deviceConnection.signTransaction({
        signingMode,
        additionalWitnessPaths,
        tx: {
          network: {
            networkId,
            protocolMagic,
          },
          inputs,
          outputs,
          fee,
          ttl,
          certificates,
          withdrawals,
          auxiliaryData,
        },
      });
      return Promise.resolve(signedTransaction);
    } catch (e) {
      throw e;
    }
  });
  signTransactionTrezorChannel.onRequest((dataToSign) => {
    logging_1.logger.info(
      '[TREZOR-CONNECT] Calling TrezorConnect.cardanoSignTransaction()'
    );
    return connect_1.default.cardanoSignTransaction(dataToSign);
  });
  resetTrezorActionChannel.onRequest(async () => {
    logging_1.logger.info('[TREZOR-CONNECT] Called TrezorConnect.cancel()');
    connect_1.default.cancel('Method_Cancel');
  });
};
exports.handleHardwareWalletRequests = handleHardwareWalletRequests;
//# sourceMappingURL=getHardwareWalletChannel.js.map
