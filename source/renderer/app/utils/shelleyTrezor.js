// @flow
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { map } from 'lodash';
import {
  derivationPathToString,
  CERTIFICATE_TYPE,
} from './hardwareWalletUtils';

import type {
  CoinSelectionInput,
  CoinSelectionOutput,
  CoinSelectionCertificate,
  CoinSelectionWithdrawal,
} from '../api/transactions/types';

export const prepareTrezorInput = (input: CoinSelectionInput) => {
  return {
    path: derivationPathToString(input.derivationPath),
    prev_hash: input.id,
    prev_index: input.index,
  };
};

export const prepareTrezorOutput = (output: CoinSelectionOutput) => {
  if (output.derivationPath) {
    // Change output
    return {
      amount: output.amount.quantity.toString(),
      tokenBundle: _getAssets(output.assets),
      addressParameters: {
        addressType: 0, // BASE address
        path: derivationPathToString(output.derivationPath),
        stakingPath: "m/1852'/1815'/0'/2/0",
      },
    };
  }
  return {
    address: output.address,
    amount: output.amount.quantity.toString(),
    tokenBundle: _getAssets(output.assets),
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

// Helper Methods

const _getAssets = (assets) => {
  const constructedAssets = map(assets, (asset) => {
    return {
      policyId: asset.policyId,
      tokens: {
        assetNameBytes: asset.assetName,
        amount: asset.quantity.toString(),
      },
    };
  });
  return constructedAssets;
};
