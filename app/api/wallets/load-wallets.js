// @flow
import data from '../data';
import environment from '../../environment';

export const loadWallets = () => new Promise((resolve) => {
  let fakeRequestTime = 1000;
  if (environment.isTest()) fakeRequestTime = 0;
  setTimeout(() => { resolve(data.wallets); }, fakeRequestTime);
});
