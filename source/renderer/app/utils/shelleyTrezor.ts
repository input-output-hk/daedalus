import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { Messages } from '@trezor/transport';
import { map } from 'lodash';
import {
  derivationPathToString,
  CERTIFICATE_TYPE,
  groupTokensByPolicyId,
} from './hardwareWalletUtils';
import type {
  CoinSelectionInput,
  CoinSelectionOutput,
  CoinSelectionCertificate,
  CoinSelectionWithdrawal,
  CoinSelectionAssetsType,
} from '../api/transactions/types';

export const TrezorTransactionSigningMode = {
  ORDINARY_TRANSACTION: 0,
  POOL_REGISTRATION_AS_OWNER: 1,
};
export const toTrezorInput = (input: CoinSelectionInput) => {
  return {
    path: derivationPathToString(input.derivationPath),
    prev_hash: input.id,
    prev_index: input.index,
  };
};
export const toTrezorOutput = (output: CoinSelectionOutput) => {
  let tokenBundle = [];

  if (output.assets) {
    tokenBundle = toTokenBundle(output.assets);
  }

  if (output.derivationPath) {
    // Change output
    return {
      amount: output.amount.quantity.toString(),
      tokenBundle,
      addressParameters: {
        addressType: 0,
        // BASE address
        path: derivationPathToString(output.derivationPath),
        stakingPath: "m/1852'/1815'/0'/2/0",
      },
    };
  }

  return {
    address: output.address,
    amount: output.amount.quantity.toString(),
    tokenBundle,
  };
};
export const toTrezorCertificate = (cert: CoinSelectionCertificate) => {
  if (cert.pool) {
    return {
      type: CERTIFICATE_TYPE[cert.certificateType],
      path: derivationPathToString(cert.rewardAccountPath),
      pool: utils.buf_to_hex(utils.bech32_decodeAddress(cert.pool)),
    };
  }

  return {
    type: CERTIFICATE_TYPE[cert.certificateType],
    path: derivationPathToString(cert.rewardAccountPath),
  };
};
export const toTrezorWithdrawal = (withdrawal: CoinSelectionWithdrawal) => {
  return {
    path: derivationPathToString(withdrawal.derivationPath),
    amount: withdrawal.amount.quantity.toString(),
  };
};
export type TrezorVotingDataType = {
  address: {
    id: string;
    spendingPath: string;
  };
  votingKey: string;
  nonce: string;
};

export const toTrezorAuxiliaryData = ({
  address,
  votingKey,
  nonce,
}: TrezorVotingDataType) => ({
  cVoteRegistrationParameters: {
    votePublicKey: votingKey,
    stakingPath: "m/1852'/1815'/0'/2/0",
    paymentAddressParameters: {
      addressType: Messages.CardanoAddressType.BASE,
      path: `m/${address.spendingPath}`,
      stakingPath: "m/1852'/1815'/0'/2/0",
    },
    nonce,
    format: Messages.CardanoCVoteRegistrationFormat.CIP15, // Catalyst voting format
  },
});
// Helper Methods
export const toTokenBundle = (assets: CoinSelectionAssetsType) => {
  const tokenObject = groupTokensByPolicyId(assets);
  const tokenObjectEntries = Object.entries(tokenObject);
  const tokenBundle = map(tokenObjectEntries, ([policyId, tokens]) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'map' does not exist on type 'unknown'.
    const tokenAmounts = tokens.map(({ assetName, quantity }) => ({
      assetNameBytes: assetName,
      amount: quantity.toString(),
    }));
    return {
      policyId,
      tokenAmounts,
    };
  });
  return tokenBundle;
};
