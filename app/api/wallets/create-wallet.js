// @flow
import faker from 'faker';

export const createPersonalWallet = (data: {
  name: string,
  currency: string
}) => new Promise((resolve) => {
  resolve(Object.assign({}, data, {
    type: 'personal',
    address: faker.finance.bitcoinAddress(),
    amount: 0,
  }));
});
