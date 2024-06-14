import { map } from 'lodash';
import blakejs from 'blakejs';
import { encode } from 'borc';
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { base58_decode } from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import { AddressStyles } from '../domains/WalletAddress';
import {
  derivationPathToLedgerPath,
  groupTokensByPolicyId,
} from './hardwareWalletUtils';

import type { AddressStyle } from '../api/addresses/types';
import type {
  CoinSelectionInput,
  CoinSelectionWithdrawal,
  CoinSelectionOutput,
  CoinSelectionAssetsType,
} from '../api/transactions/types';

import type {
  BIP32Path,
  Certificate,
} from '../../../common/types/hardware-wallets.types';

export type RewardDestinationAddressType = {
  address: {
    id: string;
    spendingPath: string;
  };
  stakingPath: BIP32Path;
};

export type TxAuxiliaryData = {
  nonce: number;
  rewardDestinationAddress: RewardDestinationAddressType;
  stakePubKey: string;
  type: 'CATALYST_VOTING';
  votingPubKey: string;
};

export type TxInputType = {
  coins: number;
  address: string;
  txid: string;
  outputNo: number;
  encodeCBOR: (...args: Array<any>) => any;
};

export type TxOutputType = {
  address: string;
  coins: number | [number, Map<Buffer, Map<Buffer, number>>];
  isChange: boolean;
  spendingPath: BIP32Path | null | undefined;
  stakingPath: BIP32Path | null | undefined;
  encodeCBOR: (...args: Array<any>) => any;
};

export type TxFeeType = {
  fee: number;
  encodeCBOR: (...args: Array<any>) => any;
};
export type TxTtlType = {
  ttl: number;
  encodeCBOR: (...args: Array<any>) => any;
};

export type TxWitnessType = {
  publicKey: string;
  signature: Buffer;
  encodeCBOR: (...args: Array<any>) => any;
};

export type TxWithdrawalsType = {
  withdrawals: Array<CoinSelectionWithdrawal>;
  encodeCBOR: (...args: Array<any>) => any;
};

export type TxBodyType = {
  getId: (...args: Array<any>) => any;
  inputs: Array<TxInputType>;
  outputs: Array<TxOutputType>;
  fee: TxFeeType;
  ttl: TxTtlType;
  certs: Array<Certificate | null | undefined>;
  withdrawals: TxWithdrawalsType | null | undefined;
  encodeCBOR: (...args: Array<any>) => any;
};

export type CborizedVotingRegistrationMetadata = [
  Map<number, Map<number, Buffer | number>>,
  []
];

export const toTxInput = (utxoInput: CoinSelectionInput) => {
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

export function toTxOutput(
  output: CoinSelectionOutput,
  addressStyle: AddressStyle
) {
  const { address, amount, derivationPath, assets } = output;
  const adaCoinQuantity = amount.quantity;
  const coins =
    assets && assets.length > 0
      ? [adaCoinQuantity, toTxOutputAssets(assets)]
      : adaCoinQuantity;

  function encodeCBOR(encoder: any) {
    const addressBuff =
      addressStyle === AddressStyles.ADDRESS_SHELLEY
        ? utils.bech32_decodeAddress(address)
        : base58_decode(address);
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
}

export const toTxFee = (fee: number) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny(fee);
  }

  return {
    fee,
    encodeCBOR,
  };
};
export const toTxTtl = (ttl: number) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny(ttl);
  }

  return {
    ttl,
    encodeCBOR,
  };
};

export const toTxOutputAssets = (assets: CoinSelectionAssetsType) => {
  const policyIdMap = new Map<Buffer, Map<Buffer, number>>();
  const tokenObject = groupTokensByPolicyId(assets);
  Object.entries(tokenObject).forEach(([policyId, tokens]) => {
    const assetMap = new Map<Buffer, number>();

    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    map(tokens, (token) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'assetName' does not exist on type 'unkno... Remove this comment to see the full error message
      assetMap.set(Buffer.from(token.assetName, 'hex'), token.quantity);
    });

    policyIdMap.set(Buffer.from(policyId, 'hex'), assetMap);
  });
  return policyIdMap;
};

export function toTxCertificate(cert: {
  type: string;
  accountAddress: string;
  pool: string | null | undefined;
}) {
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
}

export const toTxWithdrawal = (withdrawals: Array<CoinSelectionWithdrawal>) => {
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

export const toTxWitness = (publicKey: Buffer, signature: Buffer) => {
  function encodeCBOR(encoder: any) {
    return encoder.pushAny([publicKey, signature]);
  }

  return {
    publicKey,
    signature,
    encodeCBOR,
  };
};

export const toTxVotingAux = (
  txAuxiliaryData: TxAuxiliaryData,
  signatureHex: string
) => [
  // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
  new Map<number, Map<number, Buffer | number>>([
    cborizeTxVotingRegistration(txAuxiliaryData),
    [
      61285,
      new Map<number, Buffer | number>([[1, Buffer.from(signatureHex, 'hex')]]),
    ],
  ]),
  [],
];

export const TxBody = (
  inputs: Array<TxInputType>,
  outputs: Array<TxOutputType>,
  fee: TxFeeType,
  ttl: TxTtlType,
  certs: Array<Certificate | null | undefined>,
  withdrawals: TxWithdrawalsType | null | undefined,
  auxiliaryData: TxAuxiliaryData | null | undefined,
  auxiliaryDataHash: string | null | undefined
) => {
  const blake2b = (data) => blakejs.blake2b(data, null, 32);

  function getId() {
    return blake2b(
      encode(
        TxBody(
          inputs,
          outputs,
          fee,
          ttl,
          certs,
          withdrawals,
          auxiliaryData,
          auxiliaryDataHash
        )
      ) // 32
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
    if (auxiliaryDataHash) txMap.set(7, Buffer.from(auxiliaryDataHash, 'hex'));
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
    auxiliaryData,
    auxiliaryDataHash,
    encodeCBOR,
  };
};

export const cborizeTxVotingRegistration = ({
  votingPubKey,
  stakePubKey,
  rewardDestinationAddress,
  nonce,
}: TxAuxiliaryData) => {
  return [
    61284,
    new Map<number, Buffer | number>([
      [1, Buffer.from(votingPubKey, 'hex')],
      [2, Buffer.from(stakePubKey, 'hex')],
      [3, utils.bech32_decodeAddress(rewardDestinationAddress.address.id)],
      [4, Number(nonce)],
    ]),
  ];
};

export const toTxBody = ({
  txInputs,
  txOutputs,
  fee,
  ttl,
  certificates,
  withdrawals,
  txAuxiliaryData,
  txAuxiliaryDataHash,
}: {
  txInputs: Array<TxInputType>;
  txOutputs: Array<TxOutputType>;
  txAuxiliaryData?: TxAuxiliaryData;
  txAuxiliaryDataHash?: string;
  fee: number;
  ttl: number;
  certificates: Array<Certificate | null | undefined>;
  withdrawals: TxWithdrawalsType | null | undefined;
}) => {
  const txFee = toTxFee(fee);
  const txTtl = toTxTtl(ttl);
  const txCerts = certificates;
  const txWithdrawals = withdrawals;
  return TxBody(
    txInputs,
    txOutputs,
    txFee,
    txTtl,
    txCerts,
    txWithdrawals,
    txAuxiliaryData,
    txAuxiliaryDataHash
  );
};

// CDDL
// transaction =
//   [ transaction_body
//   , transaction_witness_set
//   , bool
//   , auxiliary_data / null
//   ]
export const getTxCBOR = (
  txBody: TxBodyType,
  txWitnesses: any,
  txAuxiliaryData: CborizedVotingRegistrationMetadata | null | undefined
) => {
  function getId() {
    return txBody.getId();
  }
  // The isValid flag was added in Alonzo era (onwards), mary era transactions only have three fields.
  // Since we are using '/v2/proxy/transactions' to send tx this flag has to be enabled to support Babbage tx format.
  const isValid = true;
  function encodeCBOR(encoder: any) {
    return encoder.pushAny([txBody, txWitnesses, isValid, txAuxiliaryData]);
  }

  const tx = {
    getId,
    txWitnesses,
    txBody,
    txAuxiliaryData,
    isValid,
    encodeCBOR,
  };
  return encode(tx).toString('hex');
};
