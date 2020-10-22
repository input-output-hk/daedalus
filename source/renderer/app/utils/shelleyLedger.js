// @flow
import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { encode } from 'borc';
import blakejs from 'blakejs';

// @TODO - Move to main process
// import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';

// Types
import type { CoinSelectionInput, CoinSelectionOutput } from '../api/transactions/types';
import type { BIP32Path, Certificate } from '../../../common/types/hardware-wallets.types';

export type ShelleyTxInputType = {
  coins: number,
  address: string,
  txid: string,
  outputNo: number,
  encodeCBOR: Function,
};

export type ShelleyTxOutputType = {
  address: string,
  coins: number,
  isChange: boolean,
  spendingPath: ?BIP32Path,
  stakingPath: ?BIP32Path,
  encodeCBOR: Function,
};

export type ShelleyFeeType = {
  fee: number,
  encodeCBOR: Function,
};

export type ShelleyTtlType = {
  ttl: number,
  encodeCBOR: Function,
};

export type ShelleyTxWitnessType = {
  publicKey: string,
  signature: Buffer,
  encodeCBOR: Function,
};

export type ShelleyTxAuxType = {
  getId: Function,
  inputs: Array<ShelleyTxInputType>,
  outputs: Array<ShelleyTxOutputType>,
  fee: ShelleyFeeType,
  ttl: ShelleyTtlType,
  certs: Array<?Certificate>,
  withdrawals: any, // @TODO - implement once delegation enabled
  encodeCBOR: Function,
};

// Constants
export const HARDENED_THRESHOLD = 0x80000000;
export const derivationScheme = {
  type: 'v2',
  ed25519Mode: 2,
  keyfileVersion: '2.0.0',
};

// Constructors
export const ShelleyTxWitnessShelley = (publicKey: string, signature: Buffer) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny([publicKey, signature]);
  }
  return {
    publicKey,
    signature,
    encodeCBOR,
  }
};

export const ShelleyTxInputFromUtxo = (utxoInput: CoinSelectionInput) => {
  const { address } = utxoInput;
  const coins = utxoInput.amount.quantity;
  const txid = utxoInput.id;
  const outputNo = utxoInput.index;
  const txHash = Buffer.from(txid, 'hex');

  function encodeCBOR(encoder: any) {
    return encoder.pushAny([txHash, outputNo]);
  }

  return {
    coins,
    address,
    txid,
    outputNo,
    encodeCBOR,
  };
};

export const ShelleyTxOutput = (output: CoinSelectionOutput, addressIndex: number, isChange: boolean) => {
  const { address, amount } = output;
  const coins = amount.quantity;

  function encodeCBOR(encoder: any) {
    const addressBuff = utils.bech32_decodeAddress(address);
    return encoder.pushAny([addressBuff, coins]);
  }
  return {
    address,
    coins,
    isChange,
    spendingPath: isChange ? [2147485500, 2147485463, 2147483648, 0, addressIndex] : null,
    stakingPath: isChange ? [2147485500, 2147485463, 2147483648, 2, 0] : null,
    encodeCBOR,
  };
}

export const ShelleyFee = (fee: number) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny(fee);
  }
  return {
    fee,
    encodeCBOR,
  };
}

export const ShelleyTtl = (ttl: number) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny(ttl);
  }
  return {
    ttl,
    encodeCBOR,
  };
};

export const ShelleyTxAux = (
  inputs: Array<ShelleyTxInputType>,
  outputs: Array<ShelleyTxOutputType>,
  fee: ShelleyFeeType,
  ttl: ShelleyTtlType,
  certs: Array<?Certificate>,
  withdrawals: any, // @TODO - implement once delegation enabled
) => {
  const blake2b = data => blakejs.blake2b(data, null, 32);
  function getId() {
    return blake2b(
      encode(ShelleyTxAux(inputs, outputs, fee, ttl, certs, withdrawals)),
      // 32
    ).toString('hex');
  }

  function encodeCBOR(encoder: any) {
    const txMap = new Map()
    txMap.set(0, inputs);
    txMap.set(1, outputs);
    txMap.set(2, fee);
    txMap.set(3, ttl);
    if (certs && certs.length) txMap.set(4, certs);
    if (withdrawals) txMap.set(5, withdrawals);
    return encoder.pushAny(txMap);
  }

  return {
    getId,
    inputs,
    outputs,
    fee,
    ttl,
    certs,
    withdrawals,
    encodeCBOR,
  };
};

export const ShelleySignedTransactionStructured = (
  txAux: ShelleyTxAuxType,
  witnesses: Map<number, ShelleyTxWitnessType>,
  meta: ?any, // @TODO - TBD once meta introduced
) => {
  function getId() {
    return txAux.getId();
  }

  function encodeCBOR(encoder: any) {
    return encoder.pushAny([txAux, witnesses, meta]);
  }

  return {
    getId,
    witnesses,
    txAux,
    encodeCBOR,
  };
};

export const CachedDeriveXpubFactory = (deriveXpubHardenedFn: Function) => {
  const derivedXpubs = {}

  const deriveXpub = async (absDerivationPath: Array<number>) => {
    const memoKey = JSON.stringify(absDerivationPath);
    let derivedXpubsMemo = await derivedXpubs[memoKey];

    if (!derivedXpubsMemo) {
      const deriveHardened = absDerivationPath.length === 0 || indexIsHardened(absDerivationPath.slice(-1)[0]);
      derivedXpubsMemo = deriveHardened
        ? deriveXpubHardenedFn(absDerivationPath)
        : deriveXpubNonhardenedFn(absDerivationPath);
    }

    /*
    * the derivedXpubs map stores promises instead of direct results
    * to deal with concurrent requests to derive the same xpub
    */
    return derivedXpubsMemo;
  }

  const deriveXpubNonhardenedFn = async (derivationPath) => {
    const lastIndex = derivationPath.slice(-1)[0];
    const parentXpub = await deriveXpub(derivationPath.slice(0, -1));
    // @TODO - remove flow fix and move deriveChildXpub to main process
    // $FlowFixMe
    return deriveChildXpub(parentXpub, lastIndex, derivationScheme.ed25519Mode); // eslint-disable-line
  }

  return deriveXpub;
}

// Helpers
export const indexIsHardened = (index: number) => {
  return index >= HARDENED_THRESHOLD;
};

export const prepareLedgerInput = (input: CoinSelectionInput, addressIndex: number = 0) => {
  return {
    txHashHex: input.id,
    outputIndex: input.index,
    path: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
  }
};

export const prepareLedgerOutput = (output: CoinSelectionOutput, addressIndex: number = 0, isChange: ?boolean = false) => {
  if (isChange) {
    return {
      addressTypeNibble: 0b0000, // TODO: get from address
      spendingPath: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
      amountStr: output.amount.quantity.toString(),
      stakingPath: cardano.str_to_path("1852'/1815'/0'/2/0"),
    };
  }
  return {
    amountStr: output.amount.quantity.toString(),
    addressHex: utils.buf_to_hex(utils.bech32_decodeAddress(output.address))
  };
};

export const prepareTxAux = ({
  txInputs,
  txOutputs,
  fee,
  ttl,
  certificates,
  withdrawals,
} : {
  txInputs: Array<ShelleyTxInputType>,
  txOutputs: Array<ShelleyTxOutputType>,
  fee: number,
  ttl: number,
  certificates: Array<?Certificate>,
  withdrawals: Array<any>,
}) => {
  const txFee = ShelleyFee(fee);
  const txTtl = ShelleyTtl(ttl);
  const txCerts = certificates; // @TODO - implement once delegation enabled
  const txWithdrawals = withdrawals[0]; // @TODO - implement once delegation enabled
  return ShelleyTxAux(txInputs, txOutputs, txFee, txTtl, txCerts, txWithdrawals);
};

export const prepareBody = (unsignedTx: ShelleyTxAuxType, txWitnesses: Map<number, ShelleyTxWitnessType>) => {
  const signedTransactionStructure = ShelleySignedTransactionStructured(unsignedTx, txWitnesses, null);
  return encode(signedTransactionStructure).toString('hex');
};
