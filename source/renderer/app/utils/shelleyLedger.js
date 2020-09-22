import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { derivePublic as deriveChildXpub } from 'cardano-crypto.js';
import { encode } from 'borc';
import blakejs from 'blakejs';

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
  const coins = utxo.coins;
  const txid = utxo.txHash;
  const outputNo = utxo.outputNo;
  const address = utxo.address;
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

export const ShelleyTxOutput = (address, coins, isChange, spendingPath = null, stakingPath = null) => {
  function encodeCBOR(encoder) {
    const addressBuff = utils.bech32_decodeAddress(address);
    return encoder.pushAny([addressBuff, coins]);
  }
  return {
    address,
    coins,
    isChange,
    spendingPath,
    stakingPath,
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

export const CachedDeriveXpubFactory = (derivationScheme, deriveXpubHardenedFn) => {
  const derivedXpubs = {}

  const deriveXpub = async (absDerivationPath) => {
    const memoKey = JSON.stringify(absDerivationPath);
    const derivedXpubsMemo = await derivedXpubs[memoKey];

    if (!derivedXpubs[memoKey]) {
      const deriveHardened = absDerivationPath.length === 0 || indexIsHardened(absDerivationPath.slice(-1)[0]);
      derivedXpubs[memoKey] = deriveHardened
        ? deriveXpubHardenedFn(absDerivationPath)
        : deriveXpubNonhardenedFn(absDerivationPath);
    }

    /*
    * the derivedXpubs map stores promises instead of direct results
    * to deal with concurrent requests to derive the same xpub
    */
    return await derivedXpubs[memoKey];
  }

  const deriveXpubNonhardenedFn = async (derivationPath) => {
    const lastIndex = derivationPath.slice(-1)[0]
    const parentXpub = await deriveXpub(derivationPath.slice(0, -1))
    return deriveChildXpub(parentXpub, lastIndex, derivationScheme.ed25519Mode);
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
      addressTypeNibble: 0, // TODO: get from address
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

export const prepareTxAux = (unsignedTx) => {
  const { inputs, outputs } = unsignedTx;
  const txInputs = inputs.map(ShelleyTxInputFromUtxo);
  const txOutputs = outputs.map(({
    address,
    coins,
    isChange,
    spendingPath,
    stakingPath,
  }) => {
    if (isChange) {
      return ShelleyTxOutput(address, coins, true, spendingPath, stakingPath);
    }
    return ShelleyTxOutput(address, coins, false);
  })

  const txCerts = [];
  const txFee = ShelleyFee(unsignedTx.fee);
  const txTtl = ShelleyTtl(unsignedTx.ttl);
  const txWithdrawals = undefined;
  return ShelleyTxAux(txInputs, txOutputs, txFee, txTtl, txCerts, txWithdrawals);
};

export const prepareBody = (unsignedTx, txWitnesses) => {
  const signedTransactionStructure = ShelleySignedTransactionStructured(unsignedTx, txWitnesses, null);
  return encode(signedTransactionStructure).toString('hex');
};
