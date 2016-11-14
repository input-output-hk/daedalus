// @flow
import sinon from 'sinon';

const shoppingWalletArguments = { name: 'Shopping wallet', currency: 'ada' };
const savingsWalletArguments = { name: 'Savings wallet', currency: 'btc' };

const shoppingWalletData = { address: '16CCkRJ8sdok7FicNuuNdNv1bG9QVegkA7' };
const savingsWalletData = { address: '1HJuiQT8g5p3QsNwuHAz4X9pNT4pAggoEn' };

const personalWalletData = { amount: 0, type: 'personal' };

export const createPersonalWallet = sinon.stub();

createPersonalWallet.withArgs(shoppingWalletArguments).yieldsAsync(null, {
  ...shoppingWalletArguments,
  ...shoppingWalletData,
  ...personalWalletData
});

createPersonalWallet.withArgs(savingsWalletArguments).yieldsAsync(null, {
  ...savingsWalletArguments,
  ...savingsWalletData,
  ...personalWalletData
});

createPersonalWallet.yieldsAsync('Error creating a wallet', null);
