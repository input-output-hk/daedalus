// @flow
import TransportNodeHid from "@ledgerhq/hw-transport-node-hid";
import AppAda, { utils } from "@cardano-foundation/ledgerjs-hw-app-cardano"; //"@cardano-foundation/ledgerjs-hw-app-cardano";
import { MainIpcChannel } from './lib/MainIpcChannel';
import { GET_HARDWARE_WALLET_TRANSPORT_CHANNEL, GET_EXTENDED_PUBLIC_KEY_CHANNEL, GET_CARDANO_ADA_APP_CHANNEL } from '../../common/ipc/api';
import type {
  getHardwareWalletTransportRendererRequest,
  getHardwareWalletTransportMainResponse,
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


const connectedDevice = 'xxx';
class EventObserver {
  constructor() {
    this.observers = [];
    this.connectedDevice = 'aaa';
  }
  next(eventText) {
    console.log('>> NEXT - EVENT:', eventText.type, eventText.device.productId);
    if (eventText.type === 'add') {
      console.debug('>>> SET CONNECTED DEVICE: ', eventText);
      console.debug('>>> CURRENT: ', this.connectedDevice);
      this.connectedDevice = eventText.device.productId;
      console.debug('>>>> THIS: ', this);
    }
  }
  error(e) {
    console.debug('>>> !!!!!! ERROR: ', e);
  }
  complete(params) {
    console.debug('>>> !!!!!! COMPLETE: ', params);
  }
}

export const handleHardwareWalletRequests = async () => {
  // console.debug('>>> INITIATE START ');
  // const hw = TransportNodeHid.create();
  // console.debug('>>> INITIATE: ', hw);
  console.debug('>>> observer INSTANCE');
  const observer = new EventObserver();
  console.debug('>>> listener INSTANCE');
  const listener = await TransportNodeHid.listen(observer);
  let opaa = null;
  getHardwareWalletTransportChannel.onRequest(
    async () => {

    if (opaa) {
      console.debug('>>>> INIT: ', opaa.transport.disconnected);
    }

    console.debug('>>> transport - OBS: ', observer);
    console.debug('>>> transport - OBS - ID: ', observer.connectedDevice);
    console.debug('>>> transport - listen: ', listener);
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



       console.debug('----- DONE ----: ', connectedDevice);

       console.debug('>>> getCardanoAdaAppChannel');
       const appVersion = await opaa.getVersion();
       console.debug('>>> APP version: ', appVersion);
       return Promise.resolve({
          ...hw,
          connectedDeviceId: observer.connectedDevice,
       });
    } catch (error) {
       console.debug('>>> IPC::getHardwareWalletTransportChannel - Error()', error);
       throw error;
     }
    }
  );
  getCardanoAdaAppChannel.onRequest(
    async (params) => {
      const transportList = await TransportNodeHid.list();
      console.debug('>>> transport - OBS appAda: ', opaa);
      console.debug('>>> transport - OBS list: ', transportList);
      console.debug('>>> PARAMS: ', params);

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
        const extendedPublicKey = opaa.getExtendedPublicKey(params.path);
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
};
