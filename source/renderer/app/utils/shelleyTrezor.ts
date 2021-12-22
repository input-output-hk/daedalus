import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
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
export const prepareTrezorInput = (input: CoinSelectionInput) => {
  return {
    path: derivationPathToString(input.derivationPath),
    prev_hash: input.id,
    prev_index: input.index,
  };
};
export const prepareTrezorOutput = (output: CoinSelectionOutput) => {
  let tokenBundle = [];

  if (output.assets) {
    tokenBundle = prepareTokenBundle(output.assets);
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
export const prepareTrezorCertificate = (cert: CoinSelectionCertificate) => {
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
export const prepareTrezorWithdrawal = (
  withdrawal: CoinSelectionWithdrawal
) => {
  return {
    path: derivationPathToString(withdrawal.derivationPath),
    amount: withdrawal.amount.quantity.toString(),
  };
};
export type TrezorVotingDataType = {
  votingKey: string;
  nonce: string;
};
export const prepareTrezorAuxiliaryData = ({
  votingKey,
  nonce,
}: TrezorVotingDataType) => ({
  catalystRegistrationParameters: {
    votingPublicKey: votingKey,
    stakingPath: "m/1852'/1815'/0'/2/0",
    rewardAddressParameters: {
      addressType: 14,
      // REWARDS address
      path: "m/1852'/1815'/0'/2/0",
    },
    nonce,
  },
});
// Helper Methods
export const prepareTokenBundle = (assets: CoinSelectionAssetsType) => {
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
