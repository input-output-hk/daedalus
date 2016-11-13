// @flow
import { action } from 'mobx';
import state from '../state';
import { Wallet } from '../domain/Wallet';
import { WalletTransaction } from '../domain/WalletTransaction';
import * as api from '../api-stub';

const { uiStore, walletStore } = state;

export const loadWallets = action('LOAD_WALLETS', () => {
  walletStore.isLoadingWallets = true;
  api.loadWallets((error, data) => {
    if (error) {
      walletStore.errorLoadingWallets = 'Error loading wallets'; // TODO: i18n
    } else {
      for (const wallet of data) {
        const newWallet = new Wallet(wallet);
        walletStore.wallets.push(newWallet);
        if (newWallet.lastUsed) {
          uiStore.selectedWallet = newWallet;
          loadWalletTransactions(newWallet);
        }
      }
      walletStore.isLoadingWallets = false;
    }
  });
});

export const loadWalletTransactions = action('LOAD_WALLET_TRANSACTIONS', (wallet: Wallet) => {
  api.loadWalletTransactions({ address: wallet.address }, (error, transactions) => {
    if (error) {
      walletStore.errorLoadingWalletTransactions = error;
      // TODO: handling errors from backend and i18n
    } else {
      for (const transaction of transactions) {
        const newTransaction = new WalletTransaction(transaction);
        wallet.addTransaction(newTransaction);
      }
    }
  });
});

export const createPersonalWallet = action('CREATE_PERSONAL_WALLET', (newWalletData: {
  name: string,
  currency: string
}) => {
  api.createPersonalWallet(newWalletData, (error, createdWallet) => {
    if (error) {
      walletStore.errorCreatingAWallet = error; // TODO: handling errors from backend and i18n
    } else {
      const newWallet = new Wallet(createdWallet);
      walletStore.wallets.push(newWallet);
      uiStore.selectedWallet = newWallet;
      loadWalletTransactions(newWallet);
      // TODO: Navigate to newly created wallet
    }
  });
});

export const sendMoney = action('SEND_MONEY', (transactionDetails: {
  receiver: string,
  amount: string,
  currency: string,
  description: ?string
}) => {
  api.sendMoney({
    ...transactionDetails,
    amount: parseFloat(transactionDetails.amount),
    sender: uiStore.selectedWallet.address
  }, (error, transaction) => {
    if (error) {
      walletStore.errorSendingMoney = error;
    } else {
      uiStore.selectedWallet.addTransaction(new WalletTransaction(transaction));
      uiStore.router.transitionTo('/wallet/home');
    }
  });
});
