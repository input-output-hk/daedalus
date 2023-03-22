import { BrowserWindow } from 'electron';
import TransportNodeHid, {
  getDevices,
} from '@ledgerhq/hw-transport-node-hid-noevents';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { str_to_path } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import { HexString } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/types/internal';
import TrezorConnect, {
  DEVICE,
  DEVICE_EVENT,
  Features,
  Success,
  TRANSPORT,
  TRANSPORT_EVENT,
  UI,
  UI_EVENT,
  Unsuccessful,
} from '@trezor/connect';
import { find, get, includes, last, omit } from 'lodash';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
import {
  deviceDetection,
  waitForDevice,
} from './hardwareWallets/ledger/deviceDetection';
import { IpcSender } from '../../common/ipc/lib/IpcChannel';
import { logger } from '../utils/logging';
import {
  HardwareWalletTransportDeviceRequest,
  LedgerDevicePayload,
  TransportDevice,
} from '../../common/types/hardware-wallets.types';

import { HardwareWalletChannels } from './createHardwareWalletIPCChannels';
import { Device } from './hardwareWallets/ledger/deviceDetection/types';
import { DeviceDetectionPayload } from './hardwareWallets/ledger/deviceDetection/deviceDetection';
import { initTrezorConnect, reinitTrezorConnect } from '../trezor/connection';

type ListenerType = {
  unsubscribe: (...args: Array<any>) => any;
};
type ledgerStatusType = {
  listening: boolean;
  Listener: ListenerType | null;
};
export const ledgerStatus: ledgerStatusType = {
  listening: false,
  Listener: null,
};
let devicesMemo = {};

class EventObserver {
  mainWindow: IpcSender;
  getHardwareWalletConnectionChannel: HardwareWalletChannels['getHardwareWalletConnectionChannel'];

  constructor({
    mainWindow,
    getHardwareWalletConnectionChannel,
  }: {
    mainWindow: IpcSender;
    getHardwareWalletConnectionChannel: HardwareWalletChannels['getHardwareWalletConnectionChannel'];
  }) {
    this.mainWindow = mainWindow;
    this.getHardwareWalletConnectionChannel = getHardwareWalletConnectionChannel;
  }

  next = async (event: DeviceDetectionPayload) => {
    try {
      const transportList = await TransportNodeHid.list();
      const connectionChanged = event.type === 'add' || event.type === 'remove';
      logger.info(
        `[HW-DEBUG] Ledger NEXT: , ${JSON.stringify({
          event,
          transportList,
          connectionChanged,
        })}`
      );

      if (connectionChanged) {
        logger.info('[HW-DEBUG] Ledger NEXT - connection changed');
        const { device, deviceModel } = event;

        if (event.type === 'add') {
          if (!devicesMemo[device.path]) {
            logger.info('[HW-DEBUG] CONSTRUCTOR ADD');

            const walletData: LedgerDevicePayload = {
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
              const transport = await TransportNodeHid.open(device.path);
              const AdaConnection = new AppAda(transport);
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
              logger.error('[HW-DEBUG] CONSTRUCTOR error', {
                walletData,
                error,
              });
            }
          }
        } else {
          logger.info('[HW-DEBUG] CONSTRUCTOR REMOVE');
          devicesMemo = omit(devicesMemo, [device.path]);

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

        logger.info('[HW-DEBUG] CONSTRUCTOR Memo');
      } else {
        logger.info('[HW-DEBUG] Ledger NEXT - connection NOT changed');
      }
    } catch (error) {
      logger.error(`[HW-DEBUG] Error on NEXT ${JSON.stringify(error)}`);
    }
  };

  error(e) {
    logger.info('[HW-DEBUG] Ledger NEXT error');
    throw e;
  }

  complete() {
    logger.info('[HW-DEBUG] Ledger NEXT complete');
  }
}

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
  let deviceConnection = null;
  let observer;

  const resetTrezorListeners = () => {
    // Remove all listeners if exist - e.g. on app refresh
    TrezorConnect.removeAllListeners();
    // Initialize new device listeners
    TrezorConnect.on(UI_EVENT, (event) => {
      logger.info('[TREZOR-CONNECT] Received UI_EVENT: ' + event.type);

      if (event.type === UI.REQUEST_PASSPHRASE) {
        // ui-request_passphrase
        if (event.payload && event.payload.device) {
          TrezorConnect.uiResponse({
            type: UI.RECEIVE_PASSPHRASE,
            payload: {
              save: true,
              value: '',
              passphraseOnDevice: true,
            },
          });

          logger.info(
            '[TREZOR-CONNECT] Called TrezorConnect.uiResponse - requested to provide passphrase on device'
          );
        }
      }
    });
    TrezorConnect.on(TRANSPORT_EVENT, (event) => {
      if (event.type === TRANSPORT.ERROR) {
        logger.info(
          '[TREZOR-CONNECT] Received TRANSPORT_EVENT: transport-error',
          event.payload
        );

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
    TrezorConnect.on(DEVICE_EVENT, (event) => {
      logger.info('[TREZOR-CONNECT] Received DEVICE_EVENT: ' + event.type);

      const connectionChanged =
        event.type === DEVICE.CONNECT ||
        event.type === DEVICE.DISCONNECT ||
        event.type === DEVICE.CHANGED;
      const isAcquired = get(event, ['payload', 'type'], '') === 'acquired';
      const deviceError = get(event, ['payload', 'error']);

      if (deviceError) {
        throw new Error(deviceError);
      }

      if (connectionChanged && isAcquired) {
        getHardwareWalletConnectionChannel.send(
          {
            disconnected: event.type === DEVICE.DISCONNECT,
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
    logger.info('[HW-DEBUG] waitForLedgerDevicesToConnectChannel::waiting');
    const { device, deviceModel } = await waitForDevice();
    logger.info('[HW-DEBUG] waitForLedgerDevicesToConnectChannel::found');
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

  getHardwareWalletTransportChannel.onRequest(
    async (request: HardwareWalletTransportDeviceRequest) => {
      const { isTrezor, devicePath } = request;
      logger.info('[HW-DEBUG] getHardwareWalletTransportChannel', {
        devicePath,
      });
      // Connected Trezor device info
      let deviceFeatures: Unsuccessful | Success<Features>;

      if (isTrezor) {
        logger.info('[HW-DEBUG] getHardwareWalletTransportChannel::TREZOR ');

        try {
          deviceFeatures = await TrezorConnect.getFeatures({
            device: {
              path: devicePath,
            },
          });

          logger.info('[TREZOR-CONNECT] Called TrezorConnect.getFeatures()');

          if (deviceFeatures && deviceFeatures.success) {
            logger.info('[HW-DEBUG] Trezor connect success');

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
            } as TransportDevice);
          }

          throw deviceFeatures.payload; // Error is in payload
        } catch (e) {
          logger.info(
            '[HW-DEBUG] Trezor connect error: ',
            e.message || 'no message'
          );
          throw e;
        }
      }

      try {
        logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER');
        const transportList = await TransportNodeHid.list();
        let hw;
        let lastConnectedPath;
        logger.info(
          `[HW-DEBUG] getHardwareWalletTransportChannel::transportList=${JSON.stringify(
            transportList
          )}`
        );

        const openTransportLayer = async (
          pathToOpen: string,
          device: Device
        ) => {
          if (devicesMemo[pathToOpen]) {
            logger.info('[HW-DEBUG] CLOSING EXISTING TRANSPORT');
            await devicesMemo[pathToOpen].transport.close();
          }
          const transport = await TransportNodeHid.open(pathToOpen);
          hw = transport;
          lastConnectedPath = pathToOpen;

          logger.info('[HW-DEBUG] INIT NEW transport - DONE');

          deviceConnection = new AppAda(transport);
          devicesMemo[lastConnectedPath] = {
            device,
            transport: hw,
            AdaConnection: deviceConnection,
          };
        };

        if (transportList && !transportList.length) {
          // Establish connection with last device
          try {
            logger.info('[HW-DEBUG] INIT NEW transport');

            const { device } = await waitForDevice();

            await openTransportLayer(device.path, device);
          } catch (e) {
            logger.info('[HW-DEBUG] INIT NEW transport - ERROR');
            throw e;
          }
        } else if (!devicePath || !devicesMemo[devicePath]) {
          // Use first like native usb nodeHID
          lastConnectedPath = transportList[0]; // eslint-disable-line
          logger.info('[HW-DEBUG] USE First transport', { lastConnectedPath });

          if (devicesMemo[lastConnectedPath]) {
            await openTransportLayer(
              lastConnectedPath,
              devicesMemo[lastConnectedPath].device
            );
          } else {
            throw new Error('Device not connected!');
          }
        } else {
          logger.info('[HW-DEBUG] USE CURRENT CONNECTION');
          hw = devicesMemo[devicePath].transport;
          deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);
        }

        const { deviceModel } = hw;

        if (deviceModel) {
          const { id, productName } = deviceModel;
          const ledgerData: TransportDevice = {
            deviceId: null,
            // @TODO - to be defined
            deviceType: 'ledger',
            deviceModel: id,
            // e.g. nanoS
            deviceName: productName,
            // e.g. Ledger Nano S
            path: lastConnectedPath || devicePath,
          };

          logger.info(
            '[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case RESPONSE',
            { ledgerData }
          );

          return Promise.resolve(ledgerData);
        }

        throw new Error('Missing device info');
      } catch (error) {
        logger.info('[HW-DEBUG] ERROR on getHardwareWalletTransportChannel');
        throw error;
      }
    }
  );

  handleInitTrezorConnectChannel.onRequest(async () => {
    logger.info('[HW-DEBUG] INIT TREZOR');
    await initTrezorConnect();
    resetTrezorListeners();
  });

  handleInitLedgerConnectChannel.onRequest(async () => {
    logger.info('[HW-DEBUG] INIT LEDGER');
    observer = new EventObserver({
      mainWindow: (mainWindow as unknown) as IpcSender,
      getHardwareWalletConnectionChannel,
    });

    try {
      logger.info('[HW-DEBUG] OBSERVER INIT');

      const onAdd = (payload) => {
        observer.next(payload);
      };

      const onRemove = (payload) => {
        observer.next(payload);
      };

      deviceDetection(onAdd, onRemove);

      logger.info('[HW-DEBUG] OBSERVER INIT - listener started');
    } catch (e) {
      logger.info('[HW-DEBUG] OBSERVER INIT FAILED');
      ledgerStatus.listening = false;
    }
  });
  deriveXpubChannel.onRequest(async (params) => {
    const { parentXpubHex, lastIndex, derivationScheme } = params;
    const parentXpub = utils.hex_to_buf(parentXpubHex as HexString);

    try {
      const xpub = deriveChildXpub(parentXpub, lastIndex, derivationScheme);
      return utils.buf_to_hex(xpub);
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
    const spendingPath = str_to_path(spendingPathStr);
    const stakingPath = stakingPathStr ? str_to_path(stakingPathStr) : null;

    deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);

    logger.info('[HW-DEBUG] DERIVE ADDRESS');

    if (isTrezor) {
      logger.info('[TREZOR-CONNECT] Called TrezorConnect.cardanoGetAddress()');

      const result = await TrezorConnect.cardanoGetAddress({
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
        logger.error(
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
    const encodedAddress = utils.bech32_encodeAddress(
      utils.hex_to_buf(addressHex)
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
    const spendingPath = str_to_path(spendingPathStr);
    const stakingPath = stakingPathStr ? str_to_path(stakingPathStr) : null;

    try {
      deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);

      logger.info('[HW-DEBUG] SHOW ADDRESS');

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
        const deviceList = getDevices();
        const device =
          find(deviceList, ['product', product]) ||
          find(deviceList, ['path', path]);

        logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: Path not found', {
          product,
          deviceList,
          oldPath: path,
        });

        if (!device) {
          logger.info('[HW-DEBUG] Device not instantiated!', {
            path,
            devicesMemo,
          });
          // eslint-disable-next-line
          throw {
            code: 'DEVICE_NOT_CONNECTED',
          };
        }

        const newTransport = await TransportNodeHid.open(device.path);
        const newDeviceConnection = new AppAda(newTransport);

        logger.info('[HW-DEBUG] getCardanoAdaAppChannel::Use new device path', {
          product,
          device,
          newPath: device.path,
          oldPath: path,
        });

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
        logger.info('[HW-DEBUG] Device not instantiated!', {
          path,
          devicesMemo,
        });
        // eslint-disable-next-line
        throw {
          code: 'DEVICE_NOT_CONNECTED',
        };
      }

      logger.info(`[HW-DEBUG] GET CARDANO APP path:${path}`);
      deviceConnection = devicesMemo[path].AdaConnection;
      const { version } = await deviceConnection.getVersion();

      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion');
      const { serialHex } = await deviceConnection.getSerial();

      logger.info(
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
      logger.info('[HW-DEBUG] ERROR in Cardano App', {
        path,
        errorName,
        errorMessage,
        isDisconnectError,
        isDeviceDisconnected,
      });

      if (path && !isDeviceDisconnected && isDisconnectError) {
        const oldPath = path;
        const deviceMemo = devicesMemo[oldPath];
        const devicePaths: string[] = await TransportNodeHid.list();
        const hasPathChanged = !includes(devicePaths, oldPath);
        const newPath = hasPathChanged ? last(devicePaths) : oldPath;

        if (hasPathChanged) {
          logger.info(
            `[HW-DEBUG] Device path changed from ${oldPath} to ${newPath}`
          );
        }

        if (!newPath) {
          logger.info(
            '[HW-DEBUG] ERROR in Cardano App (Device paths list is empty)',
            {
              devicePaths,
              oldPath,
              newPath,
              deviceList: getDevices(),
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
        const newTransport = await TransportNodeHid.open(newPath);
        const newDeviceConnection = new AppAda(newTransport);
        const deviceList = getDevices();
        const newDevice = find(deviceList, ['path', newPath]);
        const hasDeviceChanged = newDevice.productId !== oldDevice.productId;
        // TODO: remove
        deviceConnection = newDeviceConnection;
        // Purge old device memo
        devicesMemo = omit(devicesMemo, [oldPath]);
        logger.info(
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

    logger.info('[HW-DEBUG] getExtendedPublicKeyChannel');
    const { path, isTrezor, devicePath } = params;

    try {
      if (isTrezor) {
        // We re-initialize the Trezor Connect session to give the user the chance to provide
        // a different passphrase, in case they want to switch to a different
        // hidden wallet or just if they provided a wrong one.
        await reinitTrezorConnect();
        resetTrezorListeners();

        logger.info('[TREZOR-CONNECT] Calling TrezorConnect.getFeatures()');
        const deviceFeatures = await TrezorConnect.getFeatures();

        if (deviceFeatures.success) {
          logger.info(
            '[TREZOR-CONNECT] Calling TrezorConnect.cardanoGetPublicKey()'
          );
          const extendedPublicKeyResponse = await TrezorConnect.cardanoGetPublicKey(
            {
              path: `m/${path}`,
              showOnTrezor: true,
            }
          );

          if (!extendedPublicKeyResponse.success) {
            throw extendedPublicKeyResponse.payload;
          }

          const extendedPublicKey = get(extendedPublicKeyResponse, [
            'payload',
            'node',
          ]);

          return Promise.resolve({
            publicKeyHex: extendedPublicKey.public_key,
            chainCodeHex: extendedPublicKey.chain_code,
          });
        }

        throw new Error('Trezor device not connected');
      }

      deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);

      logger.info('[HW-DEBUG] EXPORT KEY');

      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }
      const extendedPublicKey = await deviceConnection.getExtendedPublicKey({
        path: str_to_path(path),
      });
      const deviceSerial = await deviceConnection.getSerial();
      return Promise.resolve({
        publicKeyHex: extendedPublicKey.publicKeyHex,
        chainCodeHex: extendedPublicKey.chainCodeHex,
        deviceId: deviceSerial.serialHex,
      });
    } catch (error) {
      logger.info('[HW-DEBUG] EXPORT KEY ERROR', error);
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

    logger.info('[HW-DEBUG] SIGN Ledger transaction');
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
    logger.info(
      '[TREZOR-CONNECT] Calling TrezorConnect.cardanoSignTransaction()'
    );

    return TrezorConnect.cardanoSignTransaction(dataToSign);
  });

  resetTrezorActionChannel.onRequest(async () => {
    logger.info('[TREZOR-CONNECT] Called TrezorConnect.cancel()');
    TrezorConnect.cancel('Method_Cancel');
  });
};
