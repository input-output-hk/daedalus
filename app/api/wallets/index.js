// @flow

import { loadWallets } from './load-wallets';
import { createPersonalWallet } from './create-wallet';
import { loadWalletTransactions } from './load-wallet-transactions';
import { sendMoney } from './send-money';

export default {
  loadWallets,
  createPersonalWallet,
  loadWalletTransactions,
  sendMoney,
};
