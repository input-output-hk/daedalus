import { loadAccount } from './account/load-account';
import { updateName } from './account/update-name';
import { loadWallets } from './wallet/load-wallets';
import { createPersonalWallet } from './wallet/create-wallet';
import { loadWalletTransactions } from './wallet/load-wallet-transactions';
import { sendMoney } from './wallet/send-money';

export default {
  loadAccount,
  updateName,
  loadWallets,
  createPersonalWallet,
  loadWalletTransactions,
  sendMoney
};
