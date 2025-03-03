import _ from 'lodash';
import {
  AddressType,
  CIP36VoteRegistrationFormat,
  CredentialParamsType,
  DRepParams,
  DRepParamsType,
  TxAuxiliaryDataType,
  TxOutputDestinationType,
  utils,
} from '@cardano-foundation/ledgerjs-hw-app-cardano';
import {
  base58_decode,
  str_to_path,
} from '@cardano-foundation/ledgerjs-hw-app-cardano/dist/utils/address';
import { Cardano } from '@cardano-sdk/core';
import {
  CATALYST_VOTING_REGISTRATION_TYPE,
  CERTIFICATE_TYPE,
  derivationPathToLedgerPath,
  groupTokensByPolicyId,
} from './hardwareWalletUtils';
import { AddressStyles } from '../domains/WalletAddress';
import type { AddressStyle } from '../api/addresses/types';
import type {
  CoinSelectionAssetsType,
  CoinSelectionCertificate,
  CoinSelectionInput,
  CoinSelectionOutput,
  CoinSelectionWithdrawal,
} from '../api/transactions/types';
import { TxAuxiliaryData } from './dataSerialization';

export const toTokenBundle = (assets: CoinSelectionAssetsType) => {
  const tokenObject = groupTokensByPolicyId(assets);
  const tokenObjectEntries = Object.entries(tokenObject);

  const tokenBundle = _.map(tokenObjectEntries, ([policyId, tokens]) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'unknown'.
    const tokensList = tokens.map(({ assetName, quantity }) => ({
      assetNameHex: assetName,
      amount: quantity.toString(),
    }));
    return {
      policyIdHex: policyId,
      tokens: tokensList,
    };
  });

  return tokenBundle;
};

const parseVoteDelegation = (
  cert: CoinSelectionCertificate
): DRepParams | undefined => {
  if (cert.certificateType !== 'cast_vote' || !('vote' in cert))
    return undefined;

  if (cert.vote === 'abstain') {
    return {
      type: DRepParamsType.ABSTAIN,
    };
  }

  if (cert.vote === 'no_confidence') {
    return {
      type: DRepParamsType.NO_CONFIDENCE,
    };
  }

  const { type, hash } = Cardano.DRepID.toCredential(Cardano.DRepID(cert.vote));

  if (type === Cardano.CredentialType.ScriptHash) {
    return {
      type: DRepParamsType.SCRIPT_HASH,
      scriptHashHex: hash,
    };
  }

  return {
    type: DRepParamsType.KEY_HASH,
    keyHashHex: hash,
  };
};

export const toLedgerCertificate = (cert: CoinSelectionCertificate) => {
  return {
    type: CERTIFICATE_TYPE[cert.certificateType],
    params: {
      stakeCredential: {
        type: CredentialParamsType.KEY_PATH,
        keyPath: derivationPathToLedgerPath(cert.rewardAccountPath),
      },
      poolKeyHashHex: cert.pool
        ? utils.buf_to_hex(utils.bech32_decodeAddress(cert.pool))
        : null,
      dRep: parseVoteDelegation(cert),
    },
  };
};

export const toLedgerWithdrawal = (withdrawal: CoinSelectionWithdrawal) => {
  return {
    stakeCredential: {
      type: CredentialParamsType.KEY_PATH,
      keyPath: derivationPathToLedgerPath(withdrawal.derivationPath),
    },
    amount: withdrawal.amount.quantity.toString(),
  };
};

export const toLedgerInput = (input: CoinSelectionInput) => {
  return {
    txHashHex: input.id,
    outputIndex: input.index,
    path: derivationPathToLedgerPath(input.derivationPath),
  };
};

export const toLedgerOutput = (
  output: CoinSelectionOutput,
  addressStyle: AddressStyle
) => {
  const isChange = output.derivationPath !== null;
  let tokenBundle = [];

  if (output.assets) {
    tokenBundle = toTokenBundle(output.assets);
  }

  if (isChange) {
    return {
      destination: {
        type: TxOutputDestinationType.DEVICE_OWNED,
        params: {
          type: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
          params: {
            spendingPath: derivationPathToLedgerPath(output.derivationPath),
            stakingPath: str_to_path("1852'/1815'/0'/2/0"),
          },
        },
      },
      amount: output.amount.quantity.toString(),
      tokenBundle,
    };
  }

  return {
    destination: {
      type: TxOutputDestinationType.THIRD_PARTY,
      params: {
        addressHex:
          addressStyle === AddressStyles.ADDRESS_SHELLEY
            ? utils.buf_to_hex(utils.bech32_decodeAddress(output.address))
            : utils.buf_to_hex(base58_decode(output.address)),
      },
    },
    amount: output.amount.quantity.toString(),
    tokenBundle,
  };
};

export const toLedgerAuxiliaryData = (txAuxiliaryData: TxAuxiliaryData) => {
  const { votingPubKey, rewardDestinationAddress, type } = txAuxiliaryData;
  if (type === CATALYST_VOTING_REGISTRATION_TYPE) {
    return {
      type: TxAuxiliaryDataType.CIP36_REGISTRATION,
      params: {
        format: CIP36VoteRegistrationFormat.CIP_15,
        voteKeyHex: votingPubKey,
        stakingPath: rewardDestinationAddress.stakingPath,
        paymentDestination: {
          type: TxOutputDestinationType.DEVICE_OWNED,
          params: {
            type: AddressType.BASE_PAYMENT_KEY_STAKE_KEY,
            params: {
              stakingPath: rewardDestinationAddress.stakingPath,
              spendingPath: str_to_path(
                rewardDestinationAddress.address.spendingPath
              ),
            },
          },
        },
        nonce: `${txAuxiliaryData.nonce}`,
      },
    };
  }

  // Regular tx has no voting metadata
  return null;
};
