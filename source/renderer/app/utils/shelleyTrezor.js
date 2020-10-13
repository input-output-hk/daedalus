// @flow
import { derivationPathToString } from './hardwareWalletUtils';
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