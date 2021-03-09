// @flow
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { encode } from 'borc';
import blakejs from 'blakejs';
import { map } from 'lodash';
import {
  derivationPathToLedgerPath,
  CERTIFICATE_TYPE,
} from './hardwareWalletUtils';
import { deriveXpubChannel } from '../ipc/getHardwareWalletChannel';
import { AddressStyles } from '../domains/WalletAddress';

// Types
import type {
  CoinSelectionInput,
  CoinSelectionOutput,
  CoinSelectionCertificate,
  CoinSelectionWithdrawal,
} from '../api/transactions/types';
import type {
  BIP32Path,
  Certificate,
} from '../../../common/types/hardware-wallets.types';
import type { AddressStyle } from '../api/addresses/types';

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
  withdrawals: ?ShelleyTxWithdrawalsType,
  encodeCBOR: Function,
};

export type ShelleyTxWithdrawalsType = {
  withdrawals: Array<CoinSelectionWithdrawal>,
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
export const ShelleyTxWitnessShelley = (
  publicKey: string,
  signature: Buffer
) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny([publicKey, signature]);
  }
  return {
    publicKey,
    signature,
    encodeCBOR,
  };
};

export const ShelleyTxInputFromUtxo = (utxoInput: CoinSelectionInput) => {
  const { address, amount, id, index } = utxoInput;
  const coins = amount.quantity;
  const outputNo = index;
  const txHash = Buffer.from(id, 'hex');

  function encodeCBOR(encoder: any) {
    return encoder.pushAny([txHash, outputNo]);
  }

  return {
    txid: id,
    coins,
    address,
    outputNo,
    encodeCBOR,
  };
};

// @TODO - check if this is correct multiassets constructor
// const _getAssetsMap = (adaCoinQuantity, multiassets) => {
//   const assetsMap = new Map();
//   assetsMap.set(0, adaCoinQuantity);
//   map(multiassets, (asset) => {
//     assetsMap.set(asset.policyId, { [asset.assetName]: asset.quantity })
//   })
//   return assetsMap;
// }

export const groupTokensByPolicyId = (assets) => {
  console.debug('>>> GROUP: ', assets);
  return _(assets)
    .groupBy(({ policy_id }) => policy_id)
    .value()
}

export const ShelleyTxOutputAssets = (assets) => {
  const policyIdMap = new Map<Buffer, Map<Buffer, number>>()

  // const constructedAssets = _getAssets(assets);
  const tokenObject = groupTokensByPolicyId(assets);
  console.debug('>>> GROUP - done: ', tokenObject);

  Object.entries(tokenObject).forEach(([policy_id, tokens]) => {
    console.debug('>>> MAPA: ', { policy_id, tokens })
    const assetMap = new Map<Buffer, number>()
    tokens.forEach(({asset_name, quantity}) => {
      console.debug('>>> MAPA 2: ', { asset_name, quantity })
      assetMap.set(Buffer.from(asset_name, 'hex'), quantity)
    })
    policyIdMap.set(Buffer.from(policy_id, 'hex'), assetMap)
  })
  return policyIdMap
}

export const prepareTokenBundle = (assets) => {
  console.debug('>>>> prepareTokenBundle: ', {assets});
  const tokenObject = groupTokensByPolicyId(assets)

  console.debug('>>>> prepareTokenBundle - DONE: ', tokenObject);

  return Object.entries(tokenObject).map(([policy_id, tokens]) => {
    const tokensList = tokens.map(({asset_name, quantity}) => ({
      assetNameHex: asset_name,
      amountStr: quantity.toString(),
    }));
    return {
      policyIdHex: policy_id,
      tokens: tokensList,
    }
  })
}

export const ShelleyTxOutput = (
  output: CoinSelectionOutput,
  addressStyle: AddressStyle
) => {
  const { address, amount, derivationPath } = output;
  const adaCoinQuantity = amount.quantity;

  // @TODO - check if this is correct multiassets constructor and fallback to ADA asset for now
  // const multiassets = output.assets;
  // const coins = !multiassets ? adaCoinQuantity : [adaCoinQuantity, _getAssetsMap(adaCoinQuantity, multiassets)];

  const assets = [
    {
      policy_id: "6b8d07d69639e9413dd637a1a815a7323c69c86abbafb66dbfdb1aa7",
      asset_name: '',
      quantity: 3,
    }
  ]
  console.debug('>>> START - ShelleyTxOutput: ', { assets: output.assets, LEN: output.assets.length });

  // const coins = assets.length > 0 ? [adaCoinQuantity, ShelleyTxOutputAssets(assets)] : adaCoinQuantity;
  const coins = output.assets.length > 0 ? [adaCoinQuantity, ShelleyTxOutputAssets(output.assets)] : adaCoinQuantity;


  console.debug('>>> COINS FINAL: ', coins);

  function encodeCBOR(encoder: any) {
    const addressBuff =
      addressStyle === AddressStyles.ADDRESS_SHELLEY
        ? utils.bech32_decodeAddress(address)
        : utils.base58_decode(address);
    return encoder.pushAny([addressBuff, coins]);
  }
  const isChange = derivationPath !== null;
  return {
    address,
    coins,
    isChange,
    spendingPath: isChange ? derivationPathToLedgerPath(derivationPath) : null,
    stakingPath: isChange ? [2147485500, 2147485463, 2147483648, 2, 0] : null,
    encodeCBOR,
  };
};

export const ShelleyTxCert = (cert: {
  type: string,
  accountAddress: string,
  pool: ?string,
}) => {
  const { type, accountAddress, pool } = cert;
  let hash;
  let poolHash;
  if (pool) {
    poolHash = utils.buf_to_hex(utils.bech32_decodeAddress(pool));
    hash = Buffer.from(poolHash, 'hex');
  }

  function encodeCBOR(encoder: any) {
    const accountAddressHash = utils
      .bech32_decodeAddress(accountAddress)
      .slice(1);
    const account = [0, accountAddressHash];
    const encodedCertsTypes = {
      [0]: [type, account],
      [1]: [type, account],
      [2]: [type, account, hash],
    };
    return encoder.pushAny(encodedCertsTypes[type]);
  }
  return {
    address: accountAddress,
    type,
    accountAddress,
    poolHash: poolHash || null,
    encodeCBOR,
  };
};

export const ShelleyTxWithdrawal = (
  withdrawals: Array<CoinSelectionWithdrawal>
) => {
  function encodeCBOR(encoder: any) {
    const withdrawalMap = new Map();
    map(withdrawals, (withdrawal) => {
      const rewardAccount = utils.bech32_decodeAddress(withdrawal.stakeAddress);
      const coin = withdrawal.amount.quantity;
      withdrawalMap.set(rewardAccount, coin);
    });
    return encoder.pushAny(withdrawalMap);
  }
  return {
    withdrawals,
    encodeCBOR,
  };
};

export const prepareLedgerCertificate = (cert: CoinSelectionCertificate) => {
  return {
    type: CERTIFICATE_TYPE[cert.certificateType],
    path: derivationPathToLedgerPath(cert.rewardAccountPath),
    poolKeyHashHex: cert.pool
      ? utils.buf_to_hex(utils.bech32_decodeAddress(cert.pool))
      : null,
  };
};

export const prepareLedgerWithdrawal = (
  withdrawal: CoinSelectionWithdrawal
) => {
  return {
    path: derivationPathToLedgerPath(withdrawal.derivationPath),
    amountStr: withdrawal.amount.quantity.toString(),
  };
};

export const ShelleyFee = (fee: number) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny(fee);
  }
  return {
    fee,
    encodeCBOR,
  };
};

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
  withdrawals: ?ShelleyTxWithdrawalsType
) => {
  const blake2b = (data) => blakejs.blake2b(data, null, 32);
  function getId() {
    return blake2b(
      encode(ShelleyTxAux(inputs, outputs, fee, ttl, certs, withdrawals))
      // 32
    ).toString('hex');
  }

  function encodeCBOR(encoder: any) {
    const txMap = new Map();
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
  meta: ?any // @TODO - TBD once meta introduced
) => {
  function getId() {
    return txAux.getId();
  }

  function encodeCBOR(encoder: any) {
    return encoder.pushAny([txAux, witnesses, meta]);
  }

  return {
    getId,
    encodeCBOR,
  };
};

export const CachedDeriveXpubFactory = (deriveXpubHardenedFn: Function) => {
  const derivedXpubs = {};

  const deriveXpub = async (absDerivationPath: Array<number>) => {
    const memoKey = JSON.stringify(absDerivationPath);
    let derivedXpubsMemo = await derivedXpubs[memoKey];

    if (!derivedXpubsMemo) {
      const deriveHardened =
        absDerivationPath.length === 0 ||
        indexIsHardened(absDerivationPath.slice(-1)[0]);
      derivedXpubsMemo = deriveHardened
        ? await deriveXpubHardenedFn(absDerivationPath)
        : await deriveXpubNonhardenedFn(absDerivationPath);
    }
    /*
     * the derivedXpubs map stores promises instead of direct results
     * to deal with concurrent requests to derive the same xpub
     */
    return derivedXpubsMemo;
  };

  const deriveXpubNonhardenedFn = async (derivationPath) => {
    const lastIndex = derivationPath.slice(-1)[0];
    const parentXpub = await deriveXpub(derivationPath.slice(0, -1));
    try {
      const parentXpubHex = utils.buf_to_hex(parentXpub);
      const derivedXpub = await deriveXpubChannel.request({
        parentXpubHex,
        lastIndex,
        derivationScheme: derivationScheme.ed25519Mode,
      });
      // $FlowFixMe
      return utils.hex_to_buf(derivedXpub);
    } catch (e) {
      throw e;
    }
  };

  return deriveXpub;
};

// Helpers
export const indexIsHardened = (index: number) => {
  return index >= HARDENED_THRESHOLD;
};

export const prepareLedgerInput = (input: CoinSelectionInput) => {
  return {
    txHashHex: input.id,
    outputIndex: input.index,
    path: derivationPathToLedgerPath(input.derivationPath),
  };
};

export const prepareLedgerOutput = (
  output: CoinSelectionOutput,
  addressStyle: AddressStyle
) => {
  console.debug('>>> PREPARE OUTPUT: ', output);
  const tokenBundle = prepareTokenBundle(output.assets);
  console.debug('>>> tokenBundle GROUP BY: ', tokenBundle);

  const isChange = output.derivationPath !== null;
  if (isChange) {
    return {
      addressTypeNibble: 0,
      spendingPath: derivationPathToLedgerPath(output.derivationPath),
      amountStr: output.amount.quantity.toString(),
      stakingPath: utils.str_to_path("1852'/1815'/0'/2/0"),
      // tokenBundle: output.assets ? _getAssets(output.assets) : null,
      tokenBundle,
    };
  }

  const isSheeleyAddress = addressStyle === AddressStyles.ADDRESS_SHELLEY;
  const decodedAddress = isSheeleyAddress
    ? utils.bech32_decodeAddress(output.address)
    : utils.base58_decode(output.address);
  return {
    amountStr: output.amount.quantity.toString(),
    addressHex: utils.buf_to_hex(decodedAddress),
    // tokenBundle: output.assets ? _getAssets(output.assets) : null,
    tokenBundle,
  };
};

export const prepareTxAux = ({
  txInputs,
  txOutputs,
  fee,
  ttl,
  certificates,
  withdrawals,
}: {
  txInputs: Array<ShelleyTxInputType>,
  txOutputs: Array<ShelleyTxOutputType>,
  fee: number,
  ttl: number,
  certificates: Array<?Certificate>,
  withdrawals: ?ShelleyTxWithdrawalsType,
}) => {
  const txFee = ShelleyFee(fee);
  const txTtl = ShelleyTtl(ttl);
  const txCerts = certificates;
  const txWithdrawals = withdrawals;
  return ShelleyTxAux(
    txInputs,
    txOutputs,
    txFee,
    txTtl,
    txCerts,
    txWithdrawals
  );
};

export const prepareBody = (
  unsignedTx: ShelleyTxAuxType,
  txWitnesses: any // @TODO - figure out fallback if is Map<number, ShelleyTxWitnessType> presented as empty array
) => {
  const signedTransactionStructure = ShelleySignedTransactionStructured(
    unsignedTx,
    txWitnesses,
    null
  );
  return encode(signedTransactionStructure).toString('hex');
};

// Helper Methods

const _getAssets = (assets) => {
  console.debug('>>> GET ASSETS - Ledger: ', assets);
  const constructedAssets = map(assets, (asset) => {
    return {
      policyIdHex: asset.policy_id,
      tokens: [{ // @TODO
        assetNameHex: asset.asset_name,
        amountStr: asset.quantity.toString(),
      }],
    };
  });
  console.debug('>>> GET ASSETS constructed - Ledger: ', constructedAssets);
  return constructedAssets;
};

// @TODO - check if this is correct multiassets constructor
// const _getAssetsMap = (adaCoinQuantity, multiassets) => {
//   const assetsMap = new Map();
//   assetsMap.set(0, adaCoinQuantity);
//   map(multiassets, (asset) => {
//     assetsMap.set(asset.policyId, { [asset.assetName]: asset.quantity })
//   })
//   return assetsMap;
// }
