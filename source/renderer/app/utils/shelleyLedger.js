import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { encode } from 'borc';
import blakejs from 'blakejs';
// import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';

// Constants
export const HARDENED_THRESHOLD = 0x80000000;
export const derivationScheme = {
  type: 'v2',
  ed25519Mode: 2,
  keyfileVersion: '2.0.0',
};

// Constructors
export const ShelleyTxWitnessShelley = (publicKey, signature) => {
  function encodeCBOR(encoder) {
    return encoder.pushAny([publicKey, signature]);
  }
  return {
    publicKey,
    signature,
    encodeCBOR,
  }
};

export const ShelleyTxInputFromUtxo = (utxo) => {
  const { address } = utxo;
  const coins = utxo.amount.quantity;
  const txid = utxo.id;
  const outputNo = utxo.index;
  const txHash = Buffer.from(txid, 'hex');

  function encodeCBOR(encoder) {
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

export const ShelleyTxOutput = (output, addressIndex, isChange) => {
  const { address, amount } = output;
  const coins = amount.quantity;

  function encodeCBOR(encoder) {
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

export const ShelleyFee = (fee) => {
  function encodeCBOR(encoder) {
    return encoder.pushAny(fee);
  }
  return {
    fee,
    encodeCBOR,
  };
}

export const ShelleyTtl = (ttl) => {
  function encodeCBOR(encoder) {
    return encoder.pushAny(ttl);
  }
  return {
    ttl,
    encodeCBOR,
  };
};

export const ShelleyTxAux = (inputs, outputs, fee, ttl, certs, withdrawals) => {
  const blake2b = data => blakejs.blake2b(data, null, 32);
  function getId() {
    return blake2b(
      encode(ShelleyTxAux(inputs, outputs, fee, ttl, certs, withdrawals)),
      32
    ).toString('hex');
  }

  function encodeCBOR(encoder) {
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

export const ShelleySignedTransactionStructured = (txAux, witnesses, meta) => {
  function getId() {
    return txAux.getId();
  }

  function encodeCBOR(encoder) {
    return encoder.pushAny([txAux, witnesses, meta]);
  }

  return {
    getId,
    witnesses,
    txAux,
    encodeCBOR,
  };
};

export const CachedDeriveXpubFactory = (deriveXpubHardenedFn) => {
  const derivedXpubs = {}

  const deriveXpub = async (absDerivationPath) => {
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
    // @TODO - remove flow fix and move fs to main process
    // $FlowFixMe
    return deriveChildXpub(parentXpub, lastIndex, derivationScheme.ed25519Mode); // eslint-disable-line
  }

  return deriveXpub;
}

// Helpers
export const indexIsHardened = (index) => {
  return index >= HARDENED_THRESHOLD;
};

export const prepareLedgerInput = (input, addressIndex = 0) => {
  return {
    txHashHex: input.id,
    outputIndex: input.index,
    path: cardano.str_to_path(`1852'/1815'/0'/0/${addressIndex}`),
  }
};

export const prepareLedgerOutput = (output, addressIndex = 0, isChange = false) => {
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
}) => {
  const txFee = ShelleyFee(fee);
  const txTtl = ShelleyTtl(ttl);
  const txCerts = certificates; // @TODO - implement once delegation enabled
  const txWithdrawals = withdrawals[0]; // @TODO - implement once delegation enabled
  return ShelleyTxAux(txInputs, txOutputs, txFee, txTtl, txCerts, txWithdrawals);
};

export const prepareBody = (unsignedTx, txWitnesses) => {
  const signedTransactionStructure = ShelleySignedTransactionStructured(unsignedTx, txWitnesses, null);
  return encode(signedTransactionStructure).toString('hex');
};
