// @flow
import TransportNodeHid from '@ledgerhq/hw-transport-node-hid';
import AppAda, { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import wasm from 'cardano-serialization-lib-nodejs'
import { BrowserWindow } from 'electron';
import { mnemonicToEntropy } from 'bip39';
import {
  DEVICE_EVENT,
  DEVICE,
  TRANSPORT_EVENT,
  UI_EVENT,
} from 'trezor-connect';
import { get, map } from 'lodash';
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

const TrezorConnect = require('trezor-connect').default;

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


export function normalizeToAddress(
  addr: string
): void | wasm.Address {
  // in Shelley, addresses can be base16, bech32 or base58
  // this function, we try parsing in all encodings possible

  // 1) Try converting from base58
  if (wasm.ByronAddress.is_valid(addr)) {
    return wasm.ByronAddress.from_base58(addr).to_address();
  }

  // 2) If already base16, simply return
  try {
    return wasm.Address.from_bytes(
      Buffer.from(addr, 'hex')
    );
  } catch (_e) {} // eslint-disable-line no-empty

  // 3) Try converting from base32
  try {
    return wasm.Address.from_bech32(addr);
  } catch (_e) {} // eslint-disable-line no-empty

  return undefined;
}

export function getCardanoSpendingKeyHash(
  addr: wasm.Address,
): (
  // null -> legacy address (no key hash)
  // undefined -> script hash instead of key hash
  wasm.Ed25519KeyHash | null | void
) {
  {
    const byronAddr = wasm.ByronAddress.from_address(addr);
    if (byronAddr) return null;
  }
  {
    const baseAddr = wasm.BaseAddress.from_address(addr);
    if (baseAddr) return baseAddr.payment_cred().to_keyhash();
  }
  {
    const ptrAddr = wasm.PointerAddress.from_address(addr);
    if (ptrAddr) return ptrAddr.payment_cred().to_keyhash();
  }
  {
    const enterpriseAddr = wasm.EnterpriseAddress.from_address(addr);
    if (enterpriseAddr) return enterpriseAddr.payment_cred().to_keyhash();
  }
  {
    const rewardAddr = wasm.RewardAddress.from_address(addr);
    if (rewardAddr) return rewardAddr.payment_cred().to_keyhash();
  }
  throw new Error(`${nameof(getCardanoSpendingKeyHash)} unknown address type`);
}

export function harden(num: number): number {
  return 0x80000000 + num;
}

export function getCip1852Account() {
  const entropy = mnemonicToEntropy(
    [ "test", "walk", "nut", "penalty", "hip", "pave", "soap", "entry", "language", "right", "filter", "choice" ].join(' ')
  )
  const rootKey = wasm.Bip32PrivateKey.from_bip39_entropy(
    Buffer.from(entropy, 'hex'),
    Buffer.from(''),
  );
  return rootKey
    .derive(harden(1852))
    .derive(harden(1815))
    .derive(harden(0)); // account #0
}


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

// SETUP trezor-connect
export const handleInitTrezorConnect = sender => {
  console.debug('>>> INI: ', sender);
  const initTrezorConnect = async () => {
    TrezorConnect.on(TRANSPORT_EVENT, event => {
      console.debug('>>> TRANSPORT_EVENT: ', event);
    });
    TrezorConnect.on(DEVICE_EVENT, event => {
      console.debug('>>> DEVICE_EVENT: ', event);
    });
    TrezorConnect.on(UI_EVENT, event => {
      console.debug('>>> UI_EVENT: ', event);
    });
    // TrezorConnect.init({
    //     popup: false, // render your own UI
    //     webusb: false, // webusb is not supported in electron
    //     debug: false, // see what's going on inside connect
    //     // lazyLoad: true, // set to "false" (default) if you want to start communication with bridge on application start (and detect connected device right away)
    //     // set it to "true", then trezor-connect will not be initialized until you call some TrezorConnect.method()
    //     // this is useful when you don't know if you are dealing with Trezor user
    //     manifest: {
    //         email: 'email@developer.com',
    //         appUrl: 'electron-app-boilerplate',
    //     },
    // });
    TrezorConnect.manifest({
      email: 'email@developer.com',
      appUrl: 'http://your.application.com',
    });
    TrezorConnect.init({
      popup: false, // render your own UI
      webusb: false, // webusb is not supported in electron
      debug: false, // see what's going on inside connect
      // lazyLoad: true, // set to "false" (default) if you want to start communication with bridge on application start (and detect connected device right away)
      // set it to "true", then trezor-connect will not be initialized until you call some TrezorConnect.method()
      // this is useful when you don't know if you are dealing with Trezor user
      manifest: {
        email: 'email@developer.com',
        appUrl: 'http://your.application.com',
      },
    })
      .then(res => {
        console.debug('>>> TREZOR INIT - SUCCESS: ', res);
        sender.send('trezor-connect', 'TrezorConnect is ready!');
      })
      .catch(error => {
        console.debug('>>> TREZOR INIT - ERROR ', error);
        sender.send('trezor-connect', 'TrezorConnect init error:' + error);
      });
  };

  return initTrezorConnect;

  //  TrezorConnect.init({
  //      popup: false, // render your own UI
  //      webusb: false, // webusb is not supported in electron
  //      debug: false, // see what's going on inside connect
  //      // lazyLoad: true, // set to "false" (default) if you want to start communication with bridge on application start (and detect connected device right away)
  //      // set it to "true", then trezor-connect will not be initialized until you call some TrezorConnect.method()
  //      // this is useful when you don't know if you are dealing with Trezor user
  //      manifest: {
  //          email: 'email@developer.com',
  //          appUrl: 'electron-app-boilerplate',
  //      },
  //  }).then(() => {
  //    console.debug('>>> INIT TREZOR success <<<');
  //      // sender.send('trezor-connect', 'TrezorConnect is ready!');
  //  }).catch(error => {
  //    console.debug('>>> INIT TREZOR error <<<: ', error);
  //      // sender.send('trezor-connect', 'TrezorConnect init error:' + error);
  //  });
  //  console.debug('>>> INIT TREZOR DONE');
};

export const handleHardwareWalletDevices = (mainWindow: BrowserWindow) => {
  const handleCheckHardwareWalletDevices = async () => {
    const observer = new EventObserver(mainWindow);
    await TransportNodeHid.listen(observer);
  };

  return handleCheckHardwareWalletDevices;
};

export const handleHardwareWalletRequests = async () => {
  let deviceConnection = null;
  getHardwareWalletTransportChannel.onRequest(async isTrezor => {
    console.debug('>>> ESTABLISH CONNECTION <<<');
    // console.debug('>>> TRY with TREZOR: ', TrezorConnect);

    // Connected Trezor device info
    let deviceFeatures;
    if (isTrezor) {
      try {
        console.debug('>>> ESTABLISH CONNECTION Trezor');
        deviceFeatures = await TrezorConnect.getFeatures();
      } catch (e) {
        console.debug('>>> ESTABLISH CONNECTION error: <<<', error);
      }
    }

    if (deviceFeatures && deviceFeatures.success) {
      return Promise.resolve({
        deviceID: deviceFeatures.payload.device_id,
        deviceType: 'trezor',
        deviceModel: deviceFeatures.payload.model, // e.g. "1" or "T"
        deviceName:
          deviceFeatures.payload.model === '1'
            ? 'Trezor Model One'
            : 'Trezor Model T', // @TODO - to be defined
      });
    }

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
        return Promise.resolve({
          deviceID: null, // @TODO - to be defined
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
    const { path, isTrezor } = params;

    console.debug('>>> IS TREZOR: ', isTrezor);
    /* const result = await TrezorConnect.getPublicKey({
      path: "m/44'/1815'/0'",
      showOnTrezor: true,
    }); */
    let trezorConnected = false;
    if (isTrezor) {
      const deviceFeatures = await TrezorConnect.getFeatures();
      if (deviceFeatures.success) {
        trezorConnected = true;
      }
    }
    console.debug('>>> trezorConnected: ', { trezorConnected, isTrezor });

    try {
      if (!deviceConnection && !trezorConnected) {
        throw new Error('Device not connected');
      }
      let extendedPublicKey;
      if (trezorConnected && isTrezor) {
        console.debug('>>> EXPORT TREZOR KEY <<< ');
        const extendedPublicKeyResponse = await TrezorConnect.cardanoGetPublicKey(
          {
            path: "m/1852'/1815'/0'",
            showOnTrezor: true,
          }
        );
        console.debug('>>> EXPORT RES: ', extendedPublicKeyResponse);
        if (!extendedPublicKeyResponse.success) {
          console.debug('>>> THROW ERROR: ', extendedPublicKeyResponse.payload);
          throw extendedPublicKeyResponse.payload;
        }
        console.debug('>>> SUCCESS: ', extendedPublicKeyResponse.payload);
        extendedPublicKey = get(
          extendedPublicKeyResponse,
          ['payload', 'node'],
          {}
        );
      } else {
        extendedPublicKey = await deviceConnection.getExtendedPublicKey(path);
      }
      console.debug('>>> KEY: ', extendedPublicKey);
      return Promise.resolve({
        publicKeyHex: isTrezor
          ? extendedPublicKey.public_key
          : extendedPublicKey.publicKeyHex,
        chainCodeHex: isTrezor
          ? extendedPublicKey.chain_code
          : extendedPublicKey.chainCodeHex,
      });
    } catch (error) {
      // return Promise.resolve(error);
      console.debug('>>> EXPORTING error: ', error);
      throw error;
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
    console.debug('>>> SIGN REQ received: ', {deviceConnection})
    // Comment out once Ledger integration is done

    const { inputs, outputs, transactions, protocolMagic, isTrezor, fee, ttl, networkId } = params;
    if (isTrezor) {
      console.debug('>>> Sign Transaction with >>> Trezor <<<: ', params);
      try {
        const signedTransaction = await TrezorConnect.cardanoSignTransaction({
          inputs,
          outputs,
          fee,
          ttl,
          protocolMagic,
          networkId
        });
        console.debug('>>> SUCC: ', signedTransaction);

        const serialized = signedTransaction.payload.serializedTx
        console.debug('>>> serialized: ', serialized);
        const buffer1 = Buffer.from(serialized).toString("hex");
        const buffer2 = Buffer.from(serialized, 'hex');
        console.debug('>>> Buffer 1: ', buffer1)
        console.debug('>>> Buffer 2: ', buffer2)

        return Promise.resolve({signedTransaction, buffer1, buffer2});
      } catch (e) {
        console.debug('>>> ERR: ', e);
      }
    }
    return;

    try {
      if (!deviceConnection) {
        throw new Error('Device not connected');
      }
      // const signedTransaction = await deviceConnection.signTransaction(
      //   inputs,
      //   outputs
      // );
      const {
        networkId,
        protocolMagic,
        inputs,
        outputs,
        feeStr,
        ttlStr,
        certificates,
        withdrawals,
        metadataHashHex,
        bigFee,
        coinSelection
      } = params

      console.debug('>>> TRY TO SIGN: ', params);
      const signedTransaction = await deviceConnection.signTransaction(
        networkId,
        protocolMagic,
        inputs,
        outputs,
        feeStr,
        ttlStr,
        certificates,
        withdrawals,
        metadataHashHex,
      );
      console.debug('>>> SIGNED: ', signedTransaction);
      // txBody
      /* static new(
        inputs: TransactionInputs,
        outputs: TransactionOutputs,
        fee: BigNum,
        ttl: number
      ): TransactionBody; */
      // const txBody = {
      //   inputs,
      //   outputs,
      //   fee: bigFee,
      //   ttl: 7200,
      // }


      // const txBuilder = wasm.TransactionBuilder.new(
      //   // all of these are taken from the mainnet genesis settings
      //   wasm.LinearFee.new(wasm.BigNum.from_str('44'), wasm.BigNum.from_str('155381')),
      //   wasm.BigNum.from_str('1000000'),
      //   wasm.BigNum.from_str('500000000'),
      //   wasm.BigNum.from_str('2000000')
      // );



      const protocolParams = {
        linearFee: wasm.LinearFee.new(
          wasm.BigNum.from_str('2'),
          wasm.BigNum.from_str('500'),
        ),
        minimumUtxoVal: wasm.BigNum.from_str('1'),
        poolDeposit: wasm.BigNum.from_str('500'),
        keyDeposit: wasm.BigNum.from_str('500'),
      };
      console.debug ('>>> !! protocolParams: ', protocolParams);
      const txBuilder = wasm.TransactionBuilder.new(
        protocolParams.linearFee,
        protocolParams.minimumUtxoVal,
        protocolParams.poolDeposit,
        protocolParams.keyDeposit,
      );



      // const address = wasm.ByronAddress.from_base58("Ae2tdPwUPEZLs4HtbuNey7tK4hTKrwNwYtGqp7bDfCy2WdR3P6735W5Yfpe");
      // txBuilder.add_bootstrap_input(
      //   address,
      //   wasm.TransactionInput.new(
      //     wasm.TransactionHash.from_bytes(
      //       Buffer.from("488afed67b342d41ec08561258e210352fba2ac030c98a8199bc22ec7a27ccf1", "hex"),
      //     ),
      //     0, // index
      //   ),
      //   wasm.BigNum.from_str('3000000')
      // );

      map(coinSelection.inputs, input => {
        console.debug('>>> !! Construct inputs: ', {input, normalizeToAddress, getCardanoSpendingKeyHash });
        // const address = wasm.ByronAddress.from_base58(input.address);
        // const address = wasm.ByronAddress.from_base58(input.address);

        const wasmAddr = normalizeToAddress(input.address);
        // if (wasmAddr == null) throw new Error(`Unknown address ${input.address}`);
        console.debug('>>>> wasmAddr: ', wasmAddr);
        const keyHash = getCardanoSpendingKeyHash(wasmAddr);
        console.debug('>>>> keyHash: ', keyHash);

        // console.debug('>>> !! Input Address: ', address);
        txBuilder.add_key_input(
          keyHash,
          wasm.TransactionInput.new(
            wasm.TransactionHash.from_bytes(
              Buffer.from(input.id, "hex"),
            ),
            input.index, // index
          ),
          wasm.BigNum.from_str(input.amount.quantity.toString())
        );
      })


      // txBuilder.add_output(
      //   wasm.TransactionOutput.new(
      //     address.to_address(),
      //     // we can construct BigNum (Coin) from both a js BigInt (here) or from a string (below in fee)
      //     wasm.BigNum.from_str("1000000"),
      //   ),
      // );


     // map(coinSelection.outputs, output => {
     //   console.debug('>>> !! Construct outputs: ', output);
     //   // console.debug('>>> !! output Address: ', output.address.to_address());

     //   txBuilder.add_output(
     //     wasm.TransactionOutput.new(
     //       output.address,
     //       // we can construct BigNum (Coin) from both a js BigInt (here) or from a string (below in fee)
     //       wasm.BigNum.from_str(output.amount.quantity.toString()),
     //     ),
     //   );
     // })
     map(outputs, output => {
       console.debug('>>> !! Construct outputs: ', output);
       console.debug('>>> TEST: ', wasm.Address.from_bytes(Buffer.from(output.addressHex, 'hex')));
       // console.debug('>>> !! output Address: ', output.address.to_address());
       txBuilder.add_output(
         wasm.TransactionOutput.new(
           wasm.Address.from_bytes(Buffer.from(output.addressHex, 'hex')),
           // we can construct BigNum (Coin) from both a js BigInt (here) or from a string (below in fee)
           wasm.BigNum.from_str(output.amountStr),
         ),
       );
     })


      txBuilder.set_ttl(7200);
      txBuilder.set_fee(wasm.BigNum.from_str('42'));

      console.debug('>>> !! txBuilder PASS: ', txBuilder);

      const txBody = txBuilder.build();

      console.debug('>>> TX BODY: ', txBody);

      const txHash = wasm.hash_transaction(txBody);

      console.debug('>>> TX HASH: ', txHash);


      const serializedTx = wasm.Transaction.new(
        txBody,
        signedTransaction.witnesses,
        undefined, // transaction metadata
      );
//
      console.debug('>>> serializedTx: ', serializedTx);
      const txHex = Buffer.from(serializedTx.to_bytes()).toString("hex");
      console.debug('>>> txHex: ', txHex);
      return Promise.resolve(signedTransaction);
    } catch (error) {
      console.debug('>>> ERROR OCCURED: ', error);
      throw error;
    }
  });
};
