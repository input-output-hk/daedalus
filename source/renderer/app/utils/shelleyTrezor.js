// @flow
import { utils } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import {
  derivationPathToString,
  CERTIFICATE_TYPE,
} from './hardwareWalletUtils';

import type {
  CoinSelectionInput,
  CoinSelectionOutput,
  CoinSelectionCertificate,
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
  };
};

export const prepareCertificate = (cert: CoinSelectionCertificate) => {
  return cert.pool
    ? {
        type: CERTIFICATE_TYPE[cert.certificateType],
        path: derivationPathToString(cert.rewardAccountPath),
        pool: utils.buf_to_hex(utils.bech32_decodeAddress(cert.pool)),
      }
    : {
        type: CERTIFICATE_TYPE[cert.certificateType],
        path: derivationPathToString(cert.rewardAccountPath),
      };
};
