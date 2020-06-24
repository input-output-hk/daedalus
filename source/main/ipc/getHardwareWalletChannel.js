// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { BrowserWindow } from 'electron';
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

export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  const handleCheckHardwareWalletDevices = async () => {
    const observer = new EventObserver(mainWindow);
    await TransportNodeHid.listen(observer);
  };

  return handleCheckHardwareWalletDevices;
};

export const handleHardwareWalletRequests = async () => {
  let deviceConnection = null;
  getHardwareWalletTransportChannel.onRequest(async () => {
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
      if (hw.deviceModel) {
        const { id, productName } = hw.deviceModel;
        return Promise.resolve({ id, productName });
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
      return Promise.resolve(appVersion);
    } catch (error) {
      throw error;
    }
  });
  getExtendedPublicKeyChannel.onRequest(async params => {
    const { path } = params;
    try {
      if (!deviceConnection) {
        throw new Error('Device not connected');
      }
      const extendedPublicKey = deviceConnection.getExtendedPublicKey(path);
      return Promise.resolve(extendedPublicKey);
    } catch (error) {
      return Promise.resolve(error);
    }
  });

  deriveAddressChannel.onRequest(async params => {
    // About address derivation - https://github.com/input-output-hk/cardano-wallet/wiki/About-Address-Derivation
    const derivationPath = params.derivationPath || "44'/1815'/0'/1/0";
    try {
      if (!deviceConnection) {
        throw new Error('Device not connected');
      }
      const derivedAddress = await deviceConnection.deriveAddress(
        utils.str_to_path(derivationPath)
      );
      return Promise.resolve(derivedAddress);
    } catch (error) {
      throw error;
    }
  });

  showAddressChannel.onRequest(async params => {
    const derivationPath = params.derivationPath || "44'/1815'/0'/1/0";
    try {
      if (!deviceConnection) {
        throw new Error('Device not connected');
      }
      const address = await deviceConnection.showAddress(
        utils.str_to_path(derivationPath)
      );
      return Promise.resolve(address);
    } catch (error) {
      throw error;
    }
  });

  attestUtxoChannel.onRequest(async params => {
    const { txHexData, outputIndex } = params;
    try {
      if (!deviceConnection) {
        throw new Error('Device not connected');
      }
      const utxo = await deviceConnection.attestUtxo(txHexData, outputIndex);
      return Promise.resolve(utxo);
    } catch (error) {
      throw error;
    }
  });

  signTransactionChannel.onRequest(async params => {
    const { inputs, outputs } = params;
    try {
      if (!deviceConnection) {
        throw new Error('Device not connected');
      }
      const signedTransaction = await deviceConnection.signTransaction(
        inputs,
        outputs
      );
      return Promise.resolve(signedTransaction);
    } catch (error) {
      throw error;
    }
  });
};
