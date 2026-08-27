import TransportNodeHid, {
  getDevices,
} from '@ledgerhq/hw-transport-node-hid-noevents';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { str_to_path } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import TrezorConnect, {
  DEVICE,
  DEVICE_EVENT,
  DeviceUniquePath,
  Features,
  Success,
  TRANSPORT,
  TRANSPORT_EVENT,
  UI,
  UI_EVENT,
  Unsuccessful,
} from '@trezor/connect';
import { find, get, includes, last } from 'lodash';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
import {
  deviceDetection,
  waitForDevice,
} from '../ipc/hardwareWallets/ledger/deviceDetection';
import { logger } from '../utils/logging';
import {
  HardwareWalletTransportDeviceRequest,
  LedgerDevicePayload,
  LedgerSignTransactionResponse,
  TransportDevice,
} from '../../common/types/hardware-wallets.types';

import { HardwareWalletChannels } from '../ipc/createHardwareWalletIPCChannels';
import {
  consumeIpcResponse,
  currentWindowSender,
} from '../ipc/lib/currentWindowSender';
import { Device } from '../ipc/hardwareWallets/ledger/deviceDetection/types';
import { DeviceDetectionPayload } from '../ipc/hardwareWallets/ledger/deviceDetection/deviceDetection';
import { initTrezorConnect, reinitTrezorConnect } from '../trezor/connection';

type LedgerConnection = {
  device: Device;
  transport: TransportNodeHid;
  AdaConnection: AppAda;
};

export type LedgerServiceDependencies = {
  open: (path: string) => Promise<TransportNodeHid>;
  list: () => Promise<string[]>;
  getDevices: typeof getDevices;
  detect: typeof deviceDetection;
  wait: typeof waitForDevice;
  createApp: (transport: TransportNodeHid) => AppAda;
};

const ledgerDefaults: LedgerServiceDependencies = {
  open: (path) => TransportNodeHid.open(path),
  list: () => TransportNodeHid.list(),
  getDevices,
  detect: deviceDetection,
  wait: waitForDevice,
  createApp: (transport) => new AppAda(transport),
};

const decodeHex = (value: string): Buffer => {
  if (!/^(?:[0-9a-fA-F]{2})+$/.test(value)) throw new Error('Invalid hex');
  return Buffer.from(value, 'hex');
};

export class HardwareWalletOperationCancelled extends Error {
  constructor() {
    super('Hardware wallet operation cancelled');
    this.name = 'HardwareWalletOperationCancelled';
  }
}

export class HardwareWalletService {
  private devicesMemo: Record<string, LedgerConnection> = {};
  private generations = new Map<string, number>();
  private detectorUnsubscribe: (() => void) | null = null;
  private disposed = false;

  constructor(
    private readonly ledger: LedgerServiceDependencies = ledgerDefaults
  ) {}

  private generation = (path: string): number =>
    this.generations.get(path) || 0;

  private invalidate = (path: string): void => {
    this.generations.set(path, this.generation(path) + 1);
  };

  private notifyLedger = async (
    event: DeviceDetectionPayload,
    channel: HardwareWalletChannels['getHardwareWalletConnectionChannel']
  ): Promise<void> => {
    try {
      if (this.disposed) return;
      const connectionChanged = event.type === 'add' || event.type === 'remove';
      if (!connectionChanged) return;

      const { device, deviceModel } = event;
      const walletData: LedgerDevicePayload = {
        disconnected: event.type === 'remove',
        deviceType: 'ledger',
        deviceId: null,
        deviceModel: deviceModel.id,
        deviceName: deviceModel.productName,
        path: device.path,
        product: device.product,
      };

      if (event.type === 'add') {
        if (this.devicesMemo[device.path]) return;
        const generation = this.generation(device.path);
        const transport = await this.ledger.open(device.path);
        if (this.disposed || generation !== this.generation(device.path)) {
          await transport.close();
          return;
        }
        this.devicesMemo[device.path] = {
          device,
          transport,
          AdaConnection: this.ledger.createApp(transport),
        };
      } else {
        await this.cancelLedgerOperation(device.path);
      }

      consumeIpcResponse(
        channel.send(walletData, currentWindowSender.sender),
        'GET_HARDWARE_WALLET_CONNECTION_CHANNEL'
      );
    } catch (error) {
      logger.error('[HW-DEBUG] Ledger connection event failed', {
        error: String(error),
      });
    }
  };

  private assertCurrentOperation = (
    path: string,
    record: LedgerConnection,
    generation: number
  ): void => {
    if (
      this.disposed ||
      generation !== this.generation(path) ||
      this.devicesMemo[path] !== record
    ) {
      throw new HardwareWalletOperationCancelled();
    }
  };

  public withLedgerOperation = async <T>(
    path: string,
    execute: (connection: AppAda) => Promise<T>
  ): Promise<T> => {
    const record = this.devicesMemo[path];
    if (!record) throw new Error('Ledger device not connected');
    const generation = this.generation(path);
    let result: T;
    try {
      result = await execute(record.AdaConnection);
    } catch (error) {
      this.assertCurrentOperation(path, record, generation);
      throw error;
    }
    this.assertCurrentOperation(path, record, generation);
    return result;
  };

  public cancelLedgerOperation = async (path?: string): Promise<void> => {
    const paths = path ? [path] : Object.keys(this.devicesMemo);
    await Promise.all(
      paths.map(async (devicePath) => {
        this.invalidate(devicePath);
        const record = this.devicesMemo[devicePath];
        delete this.devicesMemo[devicePath];
        if (record) {
          try {
            await record.transport.close();
          } catch (error) {
            logger.debug('[HW-DEBUG] Ledger transport close failed', {
              error: String(error),
            });
          }
        }
      })
    );
  };

  public cancelTrezorOperation = (): void => {
    TrezorConnect.cancel('Method_Cancel');
  };

  public dispose = async (): Promise<void> => {
    if (this.disposed) return;
    this.disposed = true;
    this.detectorUnsubscribe?.();
    this.detectorUnsubscribe = null;
    TrezorConnect.removeAllListeners();
    this.cancelTrezorOperation();
    await this.cancelLedgerOperation();
  };

  public register = async ({
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
  }: HardwareWalletChannels): Promise<void> => {
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
          const isNoDevicePollingNoise =
            get(event, ['payload', 'apiType']) === 'usb' &&
            get(event, ['payload', 'error']) === 'Network request failed';

          logger[isNoDevicePollingNoise ? 'debug' : 'info'](
            '[TREZOR-CONNECT] Received TRANSPORT_EVENT: transport-error',
            event.payload
          );

          // Send Transport error to Renderer
          consumeIpcResponse(
            getHardwareWalletConnectionChannel.send(
              {
                deviceType: 'trezor',
                error: {
                  payload: event.payload,
                },
              },
              currentWindowSender.sender
            ),
            'GET_HARDWARE_WALLET_CONNECTION_CHANNEL'
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
          consumeIpcResponse(
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
              currentWindowSender.sender
            ),
            'GET_HARDWARE_WALLET_CONNECTION_CHANNEL'
          );
        }
      });
    };

    waitForLedgerDevicesToConnectChannel.onRequest(async () => {
      logger.info('[HW-DEBUG] waitForLedgerDevicesToConnectChannel::waiting');
      const { device, deviceModel } = await this.ledger.wait();
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
                path: devicePath as DeviceUniquePath,
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
          const transportList = await this.ledger.list();
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
            await this.cancelLedgerOperation(pathToOpen);
            const generation = this.generation(pathToOpen);
            const transport = await this.ledger.open(pathToOpen);
            if (this.disposed || generation !== this.generation(pathToOpen)) {
              await transport.close();
              throw new HardwareWalletOperationCancelled();
            }
            hw = transport;
            lastConnectedPath = pathToOpen;
            this.devicesMemo[pathToOpen] = {
              device,
              transport,
              AdaConnection: this.ledger.createApp(transport),
            };
          };

          if (transportList && !transportList.length) {
            // Establish connection with last device
            try {
              logger.info('[HW-DEBUG] INIT NEW transport');

              const { device } = await this.ledger.wait();

              await openTransportLayer(device.path, device);
            } catch (e) {
              logger.info('[HW-DEBUG] INIT NEW transport - ERROR');
              throw e;
            }
          } else if (!devicePath || !this.devicesMemo[devicePath]) {
            // Use first like native usb nodeHID
            lastConnectedPath = transportList[0]; // eslint-disable-line
            logger.info('[HW-DEBUG] USE First transport', {
              lastConnectedPath,
            });

            if (this.devicesMemo[lastConnectedPath]) {
              await openTransportLayer(
                lastConnectedPath,
                this.devicesMemo[lastConnectedPath].device
              );
            } else {
              throw new Error('Device not connected!');
            }
          } else {
            logger.info('[HW-DEBUG] USE CURRENT CONNECTION');
            hw = this.devicesMemo[devicePath].transport;
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
      if (this.detectorUnsubscribe) return;
      try {
        const notify = (payload: DeviceDetectionPayload) => {
          this.notifyLedger(payload, getHardwareWalletConnectionChannel);
        };
        this.detectorUnsubscribe = this.ledger.detect(notify, notify);
        logger.info('[HW-DEBUG] Ledger device listener started');
      } catch (error) {
        logger.info('[HW-DEBUG] Ledger device listener failed', {
          error: String(error),
        });
        this.detectorUnsubscribe = null;
      }
    });
    deriveXpubChannel.onRequest(async (params) => {
      const { parentXpubHex, lastIndex, derivationScheme } = params;
      const parentXpub = decodeHex(parentXpubHex);

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

      logger.info('[HW-DEBUG] DERIVE ADDRESS');

      if (isTrezor) {
        logger.info(
          '[TREZOR-CONNECT] Called TrezorConnect.cardanoGetAddress()'
        );

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

      if (!devicePath) throw new Error('Ledger device not connected');
      const { addressHex } = await this.withLedgerOperation(
        devicePath,
        (connection) =>
          connection.deriveAddress({
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
          })
      );
      return utils.bech32_encodeAddress(decodeHex(addressHex));
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

      logger.info('[HW-DEBUG] SHOW ADDRESS');
      if (isTrezor) {
        throw new Error('Address verification not supported on Trezor devices');
      }
      if (!devicePath) throw new Error('Ledger device not connected');
      await this.withLedgerOperation(devicePath, (connection) =>
        connection.showAddress({
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
        })
      );
    });
    getCardanoAdaAppChannel.onRequest(async (request) => {
      const { path, product } = request;

      try {
        if (!this.devicesMemo[path]) {
          const deviceList = this.ledger.getDevices();
          const device =
            find(deviceList, ['product', product]) ||
            find(deviceList, ['path', path]);

          logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: Path not found', {
            product,
            deviceList,
            oldPath: path,
          });

          if (!device) {
            logger.info('[HW-DEBUG] Device not instantiated');
            // eslint-disable-next-line
            throw {
              code: 'DEVICE_NOT_CONNECTED',
            };
          }

          const newTransport = await this.ledger.open(device.path);
          const newDeviceConnection = this.ledger.createApp(newTransport);

          logger.info(
            '[HW-DEBUG] getCardanoAdaAppChannel::Use new device path',
            {
              product,
              device,
              newPath: device.path,
              oldPath: path,
            }
          );

          this.devicesMemo[device.path] = {
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

        if (!path || !this.devicesMemo[path]) {
          logger.info('[HW-DEBUG] Device not instantiated');
          // eslint-disable-next-line
          throw {
            code: 'DEVICE_NOT_CONNECTED',
          };
        }

        logger.info('[HW-DEBUG] GET CARDANO APP');
        const { version } = await this.withLedgerOperation(path, (connection) =>
          connection.getVersion()
        );

        logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion');
        const { minor, major, patch } = version;
        return {
          minor: String(minor),
          major: String(major),
          patch: String(patch),
          deviceId: '',
        };
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
          const deviceMemo = this.devicesMemo[oldPath];
          const devicePaths: string[] = await this.ledger.list();
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
                deviceList: this.ledger.getDevices(),
              }
            );
            // eslint-disable-next-line
            throw {
              code: 'NO_DEVICE_PATHS',
              errorCode,
              errorName,
            };
          }

          if (!deviceMemo) throw error;
          const { device: oldDevice } = deviceMemo;
          await this.cancelLedgerOperation(oldPath);
          const newTransport = await this.ledger.open(newPath);
          const newDeviceConnection = this.ledger.createApp(newTransport);
          const deviceList = this.ledger.getDevices();
          const newDevice = find(deviceList, ['path', newPath]);
          if (!newDevice) {
            await newTransport.close();
            throw error;
          }
          const hasDeviceChanged = newDevice.productId !== oldDevice.productId;
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
          this.devicesMemo[newPath] = {
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

        if (!devicePath) throw new Error('Ledger device not connected');
        logger.info('[HW-DEBUG] EXPORT KEY');
        return this.withLedgerOperation(devicePath, async (connection) => {
          const extendedPublicKey = await connection.getExtendedPublicKey({
            path: str_to_path(path),
          });
          const deviceSerial = await connection.getSerial();
          return {
            publicKeyHex: extendedPublicKey.publicKeyHex,
            chainCodeHex: extendedPublicKey.chainCodeHex,
            deviceId: deviceSerial.serialHex,
          };
        });
      } catch (error) {
        logger.info('[HW-DEBUG] EXPORT KEY ERROR');
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
      if (!devicePath) throw new Error('Device not connected!');
      return this.withLedgerOperation(devicePath, async (connection) => {
        // The trusted UI still sends the legacy reduced request; task-602 replaces it.
        const request = ({
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
        } as unknown) as Parameters<AppAda['signTransaction']>[0];
        return ((await connection.signTransaction(
          request
        )) as unknown) as LedgerSignTransactionResponse;
      });
    });

    signTransactionTrezorChannel.onRequest((dataToSign) => {
      logger.info(
        '[TREZOR-CONNECT] Calling TrezorConnect.cardanoSignTransaction()'
      );

      return TrezorConnect.cardanoSignTransaction(dataToSign);
    });

    resetTrezorActionChannel.onRequest(async () => {
      logger.info('[TREZOR-CONNECT] Called TrezorConnect.cancel()');
      this.cancelTrezorOperation();
    });
  };
}

export const hardwareWalletService = new HardwareWalletService();
