// @flow
import account from '../data/account.json';

export const loadAccount = () => new Promise((resolve) => {
  setTimeout(() => {
    resolve(account);
  }, 1000);
});
