// @flow

export const createPersonalWallet = (data: {
  name: string,
  currency: string
}) => new Promise((resolve) => {
  resolve(Object.assign({}, data, {
    type: 'personal',
    address: '16CCkRJ8sdok7FicNuuNdNv1bG9QVegkA7',
    amount: 0,
  }));
});
