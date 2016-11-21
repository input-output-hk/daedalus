import { loadAccount } from './account/load-account';
import { updateProfileField } from './account/update-profile-field';
import { loadWallets } from './wallet/load-wallets';
import { createPersonalWallet } from './wallet/create-wallet';
import { loadWalletTransactions } from './wallet/load-wallet-transactions';
import { sendMoney } from './wallet/send-money';

export default {
  loadAccount,
  updateProfileField,
  loadWallets,
  createPersonalWallet,
  loadWalletTransactions,
  sendMoney
};
