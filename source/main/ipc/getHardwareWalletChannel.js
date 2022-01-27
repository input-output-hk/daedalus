// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import { getDevices } from '@ledgerhq/hw-transport-node-hid-noevents';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { BrowserWindow } from 'electron';
import TrezorConnect, {
  DEVICE,
  DEVICE_EVENT,
  TRANSPORT,
  TRANSPORT_EVENT,
  UI,
  UI_EVENT,
  // $FlowFixMe
} from 'trezor-connect';
import { find, get, includes, last, omit } from 'lodash';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
import { MainIpcChannel } from './lib/MainIpcChannel';
import type {
  deriveAddressMainResponse,
  deriveAddressRendererRequest,
  deriveXpubMainResponse,
  deriveXpubRendererRequest,
  getCardanoAdaAppMainResponse,
  getCardanoAdaAppRendererRequest,
  getExtendedPublicKeyMainResponse,
  getExtendedPublicKeyRendererRequest,
  getHardwareWalletConnectiontMainRequest,
  getHardwareWalletConnectiontRendererResponse,
  getHardwareWalletTransportMainResponse,
  getHardwareWalletTransportRendererRequest,
  handleInitLedgerConnectMainResponse,
  handleInitLedgerConnectRendererRequest,
  handleInitTrezorConnectMainResponse,
  handleInitTrezorConnectRendererRequest,
  resetTrezorActionMainResponse,
  resetTrezorActionRendererRequest,
  showAddressMainResponse,
  showAddressRendererRequest,
  signTransactionLedgerMainResponse,
  signTransactionLedgerRendererRequest,
  signTransactionTrezorMainResponse,
  signTransactionTrezorRendererRequest,
} from '../../common/ipc/api';
import {
  DERIVE_ADDRESS_CHANNEL,
  DERIVE_XPUB_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_INIT_LEDGER_CONNECT_CHANNEL,
  GET_INIT_TREZOR_CONNECT_CHANNEL,
  RESET_ACTION_TREZOR_CHANNEL,
  SHOW_ADDRESS_CHANNEL,
  SIGN_TRANSACTION_LEDGER_CHANNEL,
  SIGN_TRANSACTION_TREZOR_CHANNEL,
} from '../../common/ipc/api';

import { logger } from '../utils/logging';
import type { HardwareWalletTransportDeviceRequest } from '../../common/types/hardware-wallets.types';

import * as CardanoSDK__Wallet from '@cardano-sdk/wallet';
import { SingleAddressWallet } from '@cardano-sdk/wallet';
import { blockfrostAssetProvider, blockfrostWalletProvider } from '@cardano-sdk/blockfrost';
import { AssetId, createStubStakePoolSearchProvider } from '@cardano-sdk/util-dev';
import JSONBigInt from 'json-bigint';

type ListenerType = {
  unsubscribe: Function,
};

type ledgerStatusType = {
  listening: boolean,
  Listener: ListenerType | null,
};

export const ledgerStatus: ledgerStatusType = {
  listening: false,
  Listener: null,
};

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

const deriveAddressChannel: MainIpcChannel<
  deriveAddressRendererRequest,
  deriveAddressMainResponse
> = new MainIpcChannel(DERIVE_ADDRESS_CHANNEL);

const showAddressChannel: MainIpcChannel<
  showAddressRendererRequest,
  showAddressMainResponse
> = new MainIpcChannel(SHOW_ADDRESS_CHANNEL);

let devicesMemo = {};
let light_wallet = {}

class EventObserver {
  constructor(props) {
    // $FlowFixMe
    this.mainWindow = props;
  }

  next = async (event) => {
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
        const device = get(event, 'device', {});
        const deviceModel = get(event, 'deviceModel', {});

        if (event.type === 'add') {
          if (!devicesMemo[device.path]) {
            logger.info('[HW-DEBUG] CONSTRUCTOR ADD');
            try {
              // $FlowFixMe
              const transport = await TransportNodeHid.open(device.path);
              const AdaConnection = new AppAda(transport);
              devicesMemo[device.path] = {
                device,
                transport,
                AdaConnection,
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
  mainWindow: BrowserWindow
) => {
  let deviceConnection = null;
  let observer;

  const resetTrezorListeners = () => {
    // Remove all listeners if exist - e.g. on app refresh
    TrezorConnect.removeAllListeners();
    // Initialize new device listeners
    TrezorConnect.on(UI_EVENT, (event) => {
      if (event.type === UI.REQUEST_PASSPHRASE) {
        // ui-request_passphrase
        if (event.payload && event.payload.device) {
          TrezorConnect.uiResponse({
            type: UI.RECEIVE_PASSPHRASE,
            payload: { value: '', passphraseOnDevice: true },
          });
        }
      }
    });
    TrezorConnect.on(TRANSPORT_EVENT, (event) => {
      if (event.type === TRANSPORT.ERROR) {
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
            deviceId: event.payload.id, // 123456ABCDEF
            deviceModel: event.payload.features.model, // e.g. T
            deviceName: event.payload.label, // e.g. Test Name
            path: event.payload.path,
            eventType: event.type,
          },
          // $FlowFixMe
          mainWindow
        );
      }
    });
  };

  getHardwareWalletTransportChannel.onRequest(
    async (request: HardwareWalletTransportDeviceRequest) => {
      const { isTrezor, devicePath } = request;
      logger.info('[HW-DEBUG] getHardwareWalletTransportChannel', devicePath);
      // Connected Trezor device info
      let deviceFeatures;
      if (isTrezor) {
        logger.info('[HW-DEBUG] getHardwareWalletTransportChannel::TREZOR ');
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

      try {
        logger.info('[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER');
        let transportList = await TransportNodeHid.list();
        let hw;
        let lastConnectedPath;

        logger.info(
          `[HW-DEBUG] getHardwareWalletTransportChannel::transportList=${JSON.stringify(
            transportList
          )}`
        );

        // $FlowFixMe
        if (transportList && !transportList.length) {
          // Establish connection with last device
          try {
            logger.info('[HW-DEBUG] INIT NEW transport');
            hw = await TransportNodeHid.create();
            transportList = await TransportNodeHid.list();
            lastConnectedPath = last(transportList);
            logger.info(
              `[HW-DEBUG] getHardwareWalletTransportChannel::lastConnectedPath=${JSON.stringify(
                lastConnectedPath
              )}`
            );

            const deviceList = getDevices();
            logger.info(
              `[HW-DEBUG] getHardwareWalletTransportChannel::deviceList=${JSON.stringify(
                deviceList
              )}`
            );

            const device = find(deviceList, ['path', lastConnectedPath]);
            logger.info('[HW-DEBUG] INIT NEW transport - DONE');

            // $FlowFixMe
            deviceConnection = new AppAda(hw);
            devicesMemo[lastConnectedPath] = {
              device,
              transport: hw,
              AdaConnection: deviceConnection,
            };
          } catch (e) {
            logger.info('[HW-DEBUG] INIT NEW transport - ERROR');
            throw e;
          }
        } else if (!devicePath || !devicesMemo[devicePath]) {
          // Use first like native usb nodeHID
          logger.info('[HW-DEBUG] USE First');
          // $FlowFixMe
          lastConnectedPath = transportList[0]; // eslint-disable-line
          if (devicesMemo[lastConnectedPath]) {
            hw = devicesMemo[lastConnectedPath].transport;
            deviceConnection = devicesMemo[lastConnectedPath].AdaConnection;
          } else {
            throw new Error('Device not connected!');
          }
        } else {
          logger.info('[HW-DEBUG] USE CURRENT CONNECTION');
          hw = devicesMemo[devicePath].transport;
          deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);
        }

        // $FlowFixMe
        const { deviceModel } = hw;
        if (deviceModel) {
          logger.info(
            '[HW-DEBUG] getHardwareWalletTransportChannel:: LEDGER case RESPONSE'
          );
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
    }
  );

  handleInitTrezorConnectChannel.onRequest(async () => {
    logger.info('[HW-DEBUG] INIT TREZOR');
    resetTrezorListeners();
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

  const networkId = 0; // TODO: Remove this
  const isTestnet = networkId === 0;
  let projectId = 'testnetQLRvAnmAEQK3q5OESvtc6ybtTtxFPA8d'// BLOCKFROST_API_KEY;

  const testSingleAddressWallet = async () => {
    logger.info('[HW-DEBUG] PERO 2: ', {
      test: CardanoSDK__Wallet
    });
    logger.info('>>> CardanoSDK__Wallet: ', CardanoSDK__Wallet);

    const { KeyManagement } = CardanoSDK__Wallet;

    /* try {
      const aa = new SingleAddressWallet()
      logger.info('>>> SingleAddressWallet: ', aa);
    } catch (e) {
      logger.info('>>> SingleAddressWallet - ERROR: ', e);
      throw e;
    } */

    const walletProps = { name: 'tomotestwallet' }; // type SingleAddressWalletProps
    // const mnemonicWords = KeyManagement.util.generateMnemonicWords();
    const mnemonicWords = [
      'immune',
      'soul',
      'bus',
      'unfair',
      'medal',
      'public',
      'bus',
      'pupil',
      'surprise',
      'wish',
      'when',
      'thunder',
      'fit',
      'youth',
      'unveil',
      'rocket',
      'suit',
      'empower',
      'heavy',
      'night',
      'voyage',
      'glare',
      'trade',
      'pyramid'
    ]; // address is "addr_test1qqnl3q7xlnzgjpyt3zy6arajjz52mkrsn3z3ld9wnckcvsyny332me0mkgnxeq7qlm223n2nj6kprsxzuzsxxj9du00ssrvqw0"
    const password = 'Secret1234';
    const keyManager = KeyManagement.createInMemoryKeyManager({
      mnemonicWords,
      networkId,
      password,
    });

    let walletProvider;
    let assetProvider;
    let stakePoolSearchProvider;
    try {
      walletProvider = blockfrostWalletProvider({ isTestnet: true, projectId });
      assetProvider = blockfrostAssetProvider({ isTestnet: true, projectId });
      stakePoolSearchProvider = createStubStakePoolSearchProvider();
      logger.info('>>>> DATA ready');
    } catch (e) {
      throw e
    }

    try {
      logger.info('>>> START - LOG walletProvider - 1 <<<');
      const networkInfo = await walletProvider.ledgerTip();
      logger.info('>>> !!! DONE 1 - walletProvider <<<: ', { ledgerTip: networkInfo, walletProvider2: JSON.stringify(walletProvider) });
      console.debug('>>>> TOMO TT 1: ', { walletProvider2: walletProvider, networkInfo })
      logger.info('>>> CONTINUE walletProvider <<<');
    } catch (e) {
      logger.info('>>> ERROR - walletProvider <<<', e);
    }

    try {
      logger.info('>>> START - LOG assetProvider - 1 <<<');
      const assetHistory = await assetProvider.getAsset('b0d07d45fe9514f80213f4020e5a61241458be626841cde717cb38a76e7574636f696e');
      logger.info('>>> !!! DONE 1 - assetProvider <<<: ', { assetHistory_1: assetHistory });
      console.debug('>>>> TOMO TT 2: ', { assetProvider22: assetProvider, assetHistory })
      logger.info('>>> CONTINUE assetProvider <<<');
    } catch (e) {
      logger.info('>>> ERROR - assetProvider <<<', e);
    }

    try {
      logger.info('>>> START - LOG stakePoolSearchProvider - 1 <<<');
      const stakePool = stakePoolSearchProvider.queryStakePools(['d-to-matc']);
      logger.info('>>> !!! DONE 1 - stakePoolSearchProvider <<<: ', { stakePool_1: stakePool, stakePoolSearchProvider });
      logger.info('>>> CONTINUE stakePoolSearchProvider <<<');
      console.debug('>>>> TOMO TT 3: ', { stakePoolSearchProvider22: stakePoolSearchProvider, stakePool })
    } catch (e) {
      logger.info('>>> ERROR - stakePoolSearchProvider <<<', e);
    }

    /* mainWindow.webContents.executeJavaScript(`
      console.log(" TOMO TOMO ${JSON.stringify(walletProvider)}");
    `) */
    // mainWindow.webContents.executeJavaScript(`console.log(walletProvider))

    /* logger.info('>>> Before Providers: ', {
      keyManager,
      walletProps,
      networkId,
      mnemonicWords,
      password,
      walletProvider,
      assetProvider,
      stakePoolSearchProvider,
    }); */

    /*
    const walletProvider = mockWalletProvider();
    const stakePoolSearchProvider = createStubStakePoolSearchProvider();
    const assetProvider = mockAssetProvider();
    const wallet = new SingleAddressWallet(walletProps, {
      assetProvider,
      keyManager,
      stakePoolSearchProvider,
      walletProvider
    });
    */

    /* let tt;
    try {
      tt = new SingleAddressWallet(walletProps, {
        assetProvider,
        keyManager,
        stakePoolSearchProvider,
        walletProvider,
      });
      logger.info('>>> Create Light Wallet - DONE');
    } catch (e) {
      logger.info('>>> Create Light Wallet - ERROR: ', e);
      throw e;
    } */
    let tt;
    try {
      logger.info('>>> START - LOG SingleAddressWallet - 1 <<<');

      /*
      beforeAll(async () => {
        wallet = new SingleAddressWallet(
          { name: 'Test Wallet' },
          {
            assetProvider,
            keyManager,
            stakePoolSearchProvider,
            walletProvider
          }
        );
        [{ rewardAccount }] = await firstValueFrom(wallet.addresses$);
      });
      */

      tt = new SingleAddressWallet(
        { name: 'Test Wallet' },
        {
          assetProvider,
          keyManager,
          stakePoolSearchProvider,
          walletProvider,
        }
      );
      console.debug('>>>> TOMO TT - Single Address Wallet: ', { SingleAddressWallet_1: tt })
      console.debug('>>>> TOMO TT - Single Address Wallet - addresses: ', { addresses: tt.addresses })
      // console.debug('>>>> TOMO TT - Single Address Wallet - addresses - value: ', { addresses: tt.addresses._value })
      console.debug('>>>> TOMO TT - Single Address Wallet - addresses - value 2: ', { addresses: tt.addresses$ })
      console.debug('>>>> TOMO TT - Single Address Wallet - addresses - value 3: ', { addresses: tt.addresses$.value })

      console.debug('>>> BALANCE 0 - ', tt.balance)
      console.debug('>>> BALANCE 1 - ', tt.balance.available)
      console.debug('>>> BALANCE 2 -', tt.balance.available$.value)
      console.debug('>>> BALANCE 3 -', tt.balance.total)
      console.debug('>>> BALANCE 4 -', tt.balance.total$)
      console.debug('>>> BALANCE 5 -', tt.balance.total$.value)

      console.debug('>>> BALANCE 6 - coins available -', tt.balance.available$.value?.coins)
      console.debug('>>> BALANCE 6 - coins total -', tt.balance.total$.value?.coins)



      console.debug('>>> TX History 1 -', tt.transactions.history)
      console.debug('>>> TX History 2 -', tt.transactions.history.all$)
      console.debug('>>> TX History 3 -', tt.transactions.history.all$.value)

      console.debug('>>> TX History 4 -', tt.transactions.history.incoming$.value)
      console.debug('>>> TX History 5 -', tt.transactions.history.all$.observers[0])
      console.debug('>>> TX History 6 -', tt.transactions.history.all$.observers[1])
      console.debug('>>> TX History 7 -', tt.transactions.history.all$.value?.length)
      console.debug('>>> TX History 8 -', tt.transactions.history.all$.observers[0].destination)
      console.debug('>>> TX History 9 -', tt.transactions.history.all$.observers[0].destination.tx)
      console.debug('>>> TX History 10 -', tt.transactions.history.all$.observers[0].destination.value)

      try {
        tt.balance.available$.subscribe(res => {
          console.debug('>>> subscribe 1 - SOMETHING HAPPENED: ', res);
        })
      } catch (e) {
        console.debug('>> subscribe 1 ERROR', e);
      }

      try {
        tt.balance.total$.subscribe(res => {
          console.debug('>>> subscribe 2 - SOMETHING HAPPENED: ', res);
        })
      } catch (e) {
        console.debug('>> subscribe 2 ERROR, e');
      }


      /* try {
        tt.balance.total$.observers[0].subscribe(res => {
          console.debug('>>> subscribe 3 - SOMETHING HAPPENED: ', res);
        })
      } catch (e) {
        console.debug('>> subscribe 3 ERROR', e);
      } */

      // logger.info('>>> !!! DONE 1 - SingleAddressWallet_1 - addresses <<<: ', { SingleAddressWallet_1: tt.addresses$.value![0] });
    } catch (e) {
      logger.info('>>> ERROR - SingleAddressWallet <<<', e);
      throw e;
    }



    logger.info('>>> !!! DONE 1 - SingleAddressWallet_1 - addresses <<<: ', { SingleAddressWallet_1: tt.addresses });
    logger.info('>>> Hello: ', {tip: tt.protocolParameters});



    /* return {
      tt,
      keyManager,
      walletProps,
      networkId,
      mnemonicWords,
      password,
      walletProvider: walletProvider(),
      assetProvider: assetProvider(),
      stakePoolSearchProvider: stakePoolSearchProvider(),
    }; */
    return {
      addresses: tt.addresses$.value,
      wallet: tt,
      balance: tt.balance.total$.value,
    };
  }

  handleInitLedgerConnectChannel.onRequest(async () => {
    logger.info('[HW-DEBUG] INIT LEDGER');
    light_wallet.one = { init: true }
    observer = new EventObserver(mainWindow);
    try {
      const fin = await testSingleAddressWallet();
      logger.info('>>> I have LW !!!');

      console.debug('>>> FIN: ', fin);

      /* logger.info('[HW-LIGHT] Response: ', {
        fin
      }); */



      logger.info('[HW-DEBUG] OBSERVER INIT');
      TransportNodeHid.setListenDevicesDebounce(1000); // Defaults to 500ms
      ledgerStatus.Listener = TransportNodeHid.listen(observer);
      ledgerStatus.listening = true;
      logger.info('[HW-DEBUG] OBSERVER INIT - listener started');
      // return JSON.stringify(LightWalletData)
      // return JSON.stringify({ done: true });
      // return fin;
      // logger.info('>>>>> DONE <<<< ', { aa: 'tt', addresses: fin.addresses });
      // return JSON.stringify(fin)
      light_wallet.one = {
        ...light_wallet.one,
        done: true,
        wallet: fin,
      }
      // return JSON.stringify({ fin, light_wallet });
      return JSON.stringify({ addresses: fin.addresses, balance: fin.balance });
    } catch (e) {
      logger.info('[HW-DEBUG] OBSERVER INIT FAILED');
      ledgerStatus.listening = false;
      throw e;
    }
  });

  deriveXpubChannel.onRequest(async (params) => {
    const { parentXpubHex, lastIndex, derivationScheme } = params;
    const parentXpub = utils.hex_to_buf(parentXpubHex);
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
    const spendingPath = utils.str_to_path(spendingPathStr);
    const stakingPath = stakingPathStr
      ? utils.str_to_path(stakingPathStr)
      : null;

    try {
      deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);
      logger.info('[HW-DEBUG] DERIVE ADDRESS');
      if (isTrezor) {
        const result = await TrezorConnect.cardanoGetAddress({
          device: {
            path: devicePath,
            showOnTrezor: true,
          },
          addressParameters: {
            addressType,
            path: `m/${spendingPathStr}`,
            stakingPath: stakingPathStr ? `m/${stakingPathStr}` : null,
          },
          protocolMagic,
          networkId,
        });
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
    } catch (e) {
      throw e;
    }
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
    const spendingPath = utils.str_to_path(spendingPathStr);
    const stakingPath = stakingPathStr
      ? utils.str_to_path(stakingPathStr)
      : null;

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
    logger.info('>>> UPDATE <<<')

    console.debug('>> OBSERVED WALLET: ', light_wallet)

    // logger.info('>>>>> DONE 22 <<<< ', { light_wallet, mainWindow });
    // mainWindow.webContents.send('>> Final', JSON.stringify(light_wallet))
    // return JSON.stringify(light_wallet);
    return JSON.stringify({
      tomo: 'test',
      addresses: light_wallet.addresses,
      balance: light_wallet.balance,
    });

    const { path } = request;
    try {
      if (!path || !devicesMemo[path]) {
        logger.info('[HW-DEBUG] Device not instantiated!', {
          path,
          devicesMemo,
        });
        // eslint-disable-next-line
        throw { code: 'DEVICE_NOT_CONNECTED' };
      }
      logger.info(`[HW-DEBUG] GET CARDANO APP path:${path}`);
      deviceConnection = devicesMemo[path].AdaConnection;
      const { version } = await deviceConnection.getVersion();
      logger.info('[HW-DEBUG] getCardanoAdaAppChannel:: appVersion');
      const { serial } = await deviceConnection.getSerial();
      logger.info(
        `[HW-DEBUG] getCardanoAdaAppChannel:: deviceSerial: ${serial}`
      );
      const { minor, major, patch } = version;
      return Promise.resolve({
        minor,
        major,
        patch,
        deviceId: serial,
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
        // $FlowFixMe
        const oldPath = path;
        const deviceMemo = devicesMemo[oldPath];
        const devicePaths = await TransportNodeHid.list();
        const hasPathChanged = !includes(devicePaths, oldPath);
        const newPath = hasPathChanged ? last(devicePaths) : oldPath;

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
          throw { code: 'NO_DEVICE_PATHS', errorCode, errorName };
        }

        const { device: oldDevice } = deviceMemo;

        // $FlowFixMe
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
          throw { code: 'DEVICE_PATH_CHANGED', path: newPath };
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
        // Check if Trezor instantiated
        const deviceFeatures = await TrezorConnect.getFeatures({
          device: { path: devicePath },
        });
        if (deviceFeatures.success) {
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

      deviceConnection = get(devicesMemo, [devicePath, 'AdaConnection']);
      logger.info('[HW-DEBUG] EXPORT KEY');

      // Check if Ledger instantiated
      if (!deviceConnection) {
        throw new Error('Ledger device not connected');
      }

      const extendedPublicKey = await deviceConnection.getExtendedPublicKey({
        path: utils.str_to_path(path),
      });
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
      validityIntervalStartStr,
      withdrawals,
      auxiliaryData,
      signingMode,
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
        withdrawals,
        validityIntervalStartStr,
        auxiliaryData,
        signingMode,
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
