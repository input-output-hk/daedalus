// @flow
import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';
import { derivationPathToString, CERTIFICATE_TYPE } from './hardwareWalletUtils';
import type { CoinSelectionInput, CoinSelectionOutput } from '../api/transactions/types';

export const prepareTrezorInput = (
  input: CoinSelectionInput,
  addressIndex: number
) => {
  return {
    path: derivationPathToString(input.derivation_path),
    prev_hash: input.id,
    prev_index: input.index,
  };
};

export const prepareTrezorOutput = (
  output: CoinSelectionOutput,
  addressIndex?: number = 0,
  isChange?: boolean = false
) => {
  if (isChange) {
    return {
      amount: output.amount.quantity.toString(),
      addressParameters: {
        addressType: 0, // BASE address
        path: `m/1852'/1815'/0'/0/${addressIndex}`,
        stakingPath: "m/1852'/1815'/0'/2/0",
      },
    };
  }
  return {
    address: output.address,
    amount: output.amount.quantity.toString(),
  }
};

export const prepareCertificate = (cert) => {
  if (cert.pool) {
    const pool11 = Buffer.from(cert.pool).toString('hex');
    const pool33 = utils.buf_to_hex(utils.bech32_decodeAddress(cert.pool))
    console.debug('>>>> HEX <<<< ', {
      pool11,
      pool33,
    })
  }
  return cert.pool
    ? {
      type: CERTIFICATE_TYPE[cert.certificate_type],
      path: derivationPathToString(cert.reward_account_path),
      pool: utils.buf_to_hex(utils.bech32_decodeAddress(cert.pool)),
    }
    : {
      type: CERTIFICATE_TYPE[cert.certificate_type],
      path: derivationPathToString(cert.reward_account_path),
    }
};
