// @flow
import TransportNodeHid from "@ledgerhq/hw-transport-node-hid";
import AppAda, { utils } from "@cardano-foundation/ledgerjs-hw-app-cardano"; //"@cardano-foundation/ledgerjs-hw-app-cardano";
import { MainIpcChannel } from './lib/MainIpcChannel';
import {
  GET_HARDWARE_WALLET_TRANSPORT_CHANNEL,
  GET_EXTENDED_PUBLIC_KEY_CHANNEL,
  GET_CARDANO_ADA_APP_CHANNEL,
  GET_HARDWARE_WALLET_CONNECTION_CHANNEL,
  DERIVE_ADDRESS_CHANNEL,
  SHOW_ADDRESS_CHANNEL,
  ATTEST_UTXO_CHANNEL,
  SIGN_TRANSACTION_CHANNEL,
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
  deriveAddressRendererRequest,
  deriveAddressMainResponse,
  showAddressRendererRequest,
  showAddresMainResponse,
  attestUtxoRendererRequest,
  attestUtxoMainResponse,
  signTransactionRendererRequest,
  signTransaMainResponse,
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

const deriveAddressChannel: MainIpcChannel<
  deriveAddressRendererRequest,
  deriveAddressMainResponse
> = new MainIpcChannel(DERIVE_ADDRESS_CHANNEL);

const showAddressChannel: MainIpcChannel<
  showAddressRendererRequest,
  showAddresMainResponse
> = new MainIpcChannel(SHOW_ADDRESS_CHANNEL);

const attestUtxoChannel: MainIpcChannel<
  attestUtxoRendererRequest,
  attestUtxoMainResponse
> = new MainIpcChannel(ATTEST_UTXO_CHANNEL);

const signTransactionChannel: MainIpcChannel<
  signTransactionRendererRequest,
  signTransaMainResponse
> = new MainIpcChannel(SIGN_TRANSACTION_CHANNEL);

const connectedDevice = 'xxx';
class EventObserver {
  constructor(props) {
    console.debug('>>> INSTANTIATING: ', props);
    this.observers = [];
    this.connectedDevice = 'aaa';
    this.test = props;
  }
  next(eventText) {
    console.debug('>>> THIS: ', this);
    console.log('>> NEXT - EVENT:', eventText.type, eventText.device.productId);
    if (eventText.type === 'add') {
      console.debug('>>> SET CONNECTED DEVICE: ', eventText);
      console.debug('>>> CURRENT: ', this.connectedDevice);
      this.connectedDevice = eventText.device.productId;
      console.debug('>>>> THIS: ', this);
      try {
        // console.debug('>>> getHardwareWalletConnectionChannel: ', getHardwareWalletConnectionChannel);
        // getHardwareWalletConnectionChannel.send({ disconnected: true });
      } catch (e) {
        console.debug('>>> ERROR - SENDER: ', e)
      }
    }
    if (eventText.type === 'remove') {
      console.debug('>>> D I S C O N N E C T <<<');
      try {
        console.debug('>>> getHardwareWalletConnectionChannel: ', getHardwareWalletConnectionChannel);
        const mainWindow = this.test;
        getHardwareWalletConnectionChannel.send({ disconnected: true }, mainWindow);
      } catch (e) {
        console.debug('>>> ERROR - SENDER: ', e)
      }
    }
  }
  error(e) {
    console.debug('>>> !!!!!! ERROR: ', e);
  }
  complete(params) {
    console.debug('>>> !!!!!! COMPLETE: ', params);
  }
}

export const handleHardwareWalletDevices = (
  mainWindow: BrowserWindow,
) => {
  console.debug('>>> HERE');
  const handleCheckHardwareWalletDevices = async () => {
    console.debug('>>> START LISTENER: ', mainWindow);
    const observer = new EventObserver(mainWindow);
    await TransportNodeHid.listen(observer);
    console.debug('>>> LISTENING...');
    // getHardwareWalletConnectionChannel.send({ disconnected: true }, mainWindow);
  }

  return handleCheckHardwareWalletDevices;
}

export const handleHardwareWalletRequests = async () => {
  // console.debug('>>> INITIATE START ');
  // const hw = TransportNodeHid.create();
  // console.debug('>>> INITIATE: ', hw);

  // console.debug('>>> observer INSTANCE');
  // const observer = new EventObserver({ddd: 'tomo'});
  // console.debug('>>> listener INSTANCE');
  // const listener = await TransportNodeHid.listen(observer);
  let opaa = null;
  getHardwareWalletTransportChannel.onRequest(
    async () => {

    if (opaa) {
      console.debug('>>>> INIT: ', opaa.transport.disconnected);
    }

    // console.debug('>>> transport - OBS: ', observer);
    // console.debug('>>> transport - OBS - ID: ', observer.connectedDevice);
    // console.debug('>>> transport - listen: ', listener);
     try {
      const transportList = await TransportNodeHid.list();
      console.debug('>>> GET HW');

      console.debug('>>>> CURRENT STATUS: ', opaa);


      let hw;
      if (!opaa || opaa.transport.disconnected) {
        if (transportList.length) {
          console.debug('>>>> OPEN <<<<<');
          hw = await TransportNodeHid.open(transportList[0]);
        } else {
          console.debug('>>>> CREATE <<<<<');
          hw = await TransportNodeHid.create();
        }
        console.debug('>>> TRANSPORT LIST: ', transportList);

         console.debug('>>> IPC::Hw', hw);
         console.debug('>>> OPAA: ', opaa);
         const appAda = new AppAda(hw);
         // const appVersion = await appAda.getVersion();
         // console.debug('>>> APP version: ', appVersion);
         opaa = appAda;
      } else {
        hw = opaa.transport;
      }



       console.debug('----- DONE ----');

       // console.debug('>>> getCardanoAdaAppChannel');
       // const appVersion = await opaa.getVersion();
       // console.debug('>>> APP version: ', appVersion);
       return Promise.resolve({
          ...hw,
          // connectedDeviceId: observer.connectedDevice,
       });
    } catch (error) {
       console.debug('>>> IPC::getHardwareWalletTransportChannel - Error()', error);
       throw error;
     }
    }
  );
  getCardanoAdaAppChannel.onRequest(
    async (params) => {
      console.debug('>>> PARAMS: ', params);
      const transportList = await TransportNodeHid.list();
      console.debug('>>> transport - OBS appAda: ', opaa);
      console.debug('>>> transport - OBS list: ', transportList);

      // If transport is initialized outside Cardano ADA app it is set to disconnected so we need to reconnect same channel
      // if (opaa.transport.disconnected) {
      try {
        console.debug('>>> OPEN NEW CONNECTION');
        const newConnection = await TransportNodeHid.open(transportList[0]);
        console.debug('>>> OPEN NEW CONNECTION - done: ', newConnection);
        opaa = new AppAda(newConnection);
      } catch (e) {
        console.debug('CONNECTION ERROR: ', e);
      }
      // }

      // const { isConnected } = params;
      // if (!isConnected) {
      //   console.debug('>>>> CREATE NEW CONNECTION');
      //   const hw = await TransportNodeHid.create();
      //   const appAda = new AppAda(hw);
      //   opaa = appAda
      // }
      // opaa = opaa;
     try {
       console.debug('>>> getCardanoAdaAppChannel');
       const appVersion = await opaa.getVersion();
       console.debug('>>> APP version: ', appVersion);
       return Promise.resolve(appVersion);
    } catch (error) {
       console.debug('>>> IPC::getCardanoAdaAppChannel - Error()', error);
       console.debug('>>> ERROR in Cardano ---', opaa)
       throw error;
     }
    }
  );
  getExtendedPublicKeyChannel.onRequest(
    async (params) => {
      const { path } = params;
      console.debug('>>> OPAA 2: ', opaa);
      try {
        // const appAda = new AppAda(opaa);
        // console.debug('>>> appAda: ', appAda);


        // console.debug('>>> get APP version <<<');
        // const appVersion = await appAda.getVersion();
        // console.debug('>>> APP version: ', appVersion);

        // console.debug('>>> INSTANCE 111: ', appAda);
        // appAda.transport.device.setNonBlocking();
        console.debug('>>> GET getExtendedPublicKey');
        // const extendedPublicKey = appAda.getExtendedPublicKey(params.path);
        const extendedPublicKey = opaa.getExtendedPublicKey(path);
        // const appVersion = await appAda.getVersion();
        console.debug('>>> extendedPublicKey 111: ', extendedPublicKey)

        return Promise.resolve(extendedPublicKey);
      } catch (error) {
        console.debug('>>> IPC::extendedPublicKey - Error()', error);
        return Promise.resolve(error);
        // throw error;
      }
    }
  );

  deriveAddressChannel.onRequest(
    async (params) => {
      const derivationPath = params.derivationPath || "44'/1815'/0'/1/0";
      console.debug('>>> DERIVE ADDRESS <<<: ', {params, opaa, derivationPath});
      // About address derivation - https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Derivation

      try {
        const derivedAddress = await opaa.deriveAddress(utils.str_to_path(derivationPath));
        console.debug('>>> derivedAddress: ', derivedAddress);
        return Promise.resolve(derivedAddress);
      } catch(error) {
        console.debug('>>> derivedAddress - ERROR: ', error);
        throw error;
      }
    }
  );

  showAddressChannel.onRequest(
    async (params) => {
      const derivationPath = params.derivationPath || "44'/1815'/0'/1/0";
      console.debug('>>> SHOW ADDRESS <<<: ', {params, opaa, derivationPath});

      try {
        const address = await opaa.showAddress(utils.str_to_path(derivationPath));
        console.debug('>>> Address: ', address);
        return Promise.resolve(address);
      } catch(error) {
        console.debug('>>> showAddress - ERROR: ', error);
        throw error;
      }
    }
  );

  attestUtxoChannel.onRequest(
    async (params) => {
      const { txHexData, outputIndex } = params;
      console.debug('>>> ATTEST UTXO <<<: ', {params, opaa});

      try {
        const utxo = await opaa.attestUtxo(txHexData, outputIndex);
        console.debug('>>> ATTEST UTXO - res: ', utxo);
        return Promise.resolve(utxo);
      } catch(error) {
        console.debug('>>> ATTEST UTXO - ERROR: ', error);
        throw error;
      }
    }
  );

  signTransactionChannel.onRequest(
    async (params) => {
      const { inputs, outputs } = params;
      console.debug('>>> SIGN TRANSACTION inputs <<<: ', inputs);
      console.debug('>>> SIGN TRANSACTION outputs <<<: ', outputs);
      try {
        const signedTransaction = await opaa.signTransaction(inputs, outputs);
        // console.log(await opaa.signTransaction(inputs, outputs));
        console.debug('>>> SIGN TRANSACTION - res: ', signedTransaction);
        return Promise.resolve(signedTransaction);
      } catch(error) {
        console.debug('>>> SIGN TRANSACTION - ERROR: ', error);
        throw error;
      }
    }
  );
};
