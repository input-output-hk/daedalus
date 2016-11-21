// @flow
import wallets from '../data/wallets.json';

export const loadWallets = () => new Promise((resolve) => {
  setTimeout(() => {
    resolve(wallets);
  }, 1000);
});
