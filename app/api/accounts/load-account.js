// @flow
import data from '../data';

export const loadAccount = () => new Promise((resolve) => {
  resolve(data.account);
});
