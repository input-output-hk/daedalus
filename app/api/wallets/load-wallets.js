// @flow
import data from '../data';

export const loadWallets = () => new Promise((resolve) => {
  setTimeout(() => {
    resolve(data.wallets);
  }, 1000);
});
