import { utils, cardano } from '@cardano-foundation/ledgerjs-hw-app-cardano';

export const prepareTrezorInput = (input, addressIndex) => {
  return {
    path: `m/1852'/1815'/0'/0/${addressIndex}`,
    prev_hash: input.id,
    prev_index: input.index,
  };
};

export const prepareTrezorOutput = (output, addressIndex = 0, isChange = false) => {
  if (isChange) {
    return {
      amount: output.amount.quantity.toString(),
      addressParameters: {
        addressType: 0, // TODO: 0 for base address
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