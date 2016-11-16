// @flow
import wallets from '../data/wallets.json';

export const loadWallets = () => new Promise((resolve) => {
  resolve(wallets);
});
