// @flow
import account from '../data/account.json';

export const loadAccount = () => new Promise((resolve) => {
  resolve(account);
});
