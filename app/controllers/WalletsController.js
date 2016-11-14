// @flow
import { action } from 'mobx';
import AppStore from '../stores/AppStore';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import api from '../api';

export default class WalletsController {

  store: AppStore;

  constructor(store: AppStore) {
    this.store = store;
  }

  @action async loadWallets() {
    const state = this.store.wallets;
    state.isLoading = true;
    try {
      const wallets = await api.loadWallets();
      for (const wallet of wallets) {
        const newWallet = new Wallet(wallet);
        this.store.account.addWallet(newWallet);
        if (newWallet.lastUsed) this.setActiveWallet(newWallet);
      }
      wallets.isLoading = false;
    } catch (error) {
      state.errorLoading = 'Error loading wallets'; // TODO: i18n
    }
  }

  @action async loadActiveWalletTransactions() {
    const state = this.store.wallets;
    try {
      const transactions = await api.loadWalletTransactions({
        address: state.activeWallet.address
      });
      for (const transaction of transactions) {
        state.activeWallet.addTransaction(new WalletTransaction(transaction));
      }
    } catch (error) {
      state.errorLoadingTransactions = error;
      // TODO: handling errors from backend and i18n
    }
  }

  @action setActiveWallet(wallet: Wallet) {
    this.store.wallets.activeWallet = wallet;
    this.loadActiveWalletTransactions();
  }

  @action async sendMoney(transactionDetails: {
    receiver: string,
    amount: string,
    description: ?string
  }) {
    const wallets = this.store.wallets;
    try {
      const transaction = api.sendMoney({
        ...transactionDetails,
        amount: parseFloat(transactionDetails.amount),
        sender: wallets.activeWallet.address,
        currency: wallets.activeWallet.currency
      });
      wallets.activeWallet.addTransaction(new WalletTransaction(transaction));
      if (this.store.router) this.store.router.transitionTo('/wallet/home');
    } catch (error) {
      wallets.errorSendingMoney = error;
    }
  }

  @action async createPersonalWallet(newWalletData: { name: string, currency: string}) {
    try {
      const createdWallet = api.createPersonalWallet(newWalletData);
      const newWallet = new Wallet(createdWallet);
      this.store.account.addWallet(newWallet);
      this.setActiveWallet(newWallet);
      // TODO: Navigate to newly created wallet
    } catch (error) {
      this.store.wallets.errorCreating = error; // TODO: handling errors from backend and i18n
    }
  }

}
