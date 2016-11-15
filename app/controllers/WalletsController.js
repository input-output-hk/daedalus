import { action } from 'mobx';
import type { appState } from '../state/index';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import api from '../api';

export default class WalletsController {

  state: appState;

  constructor(state: appState) {
    this.state = state;
  }

  @action async loadWallets() {
    const state = this.state.wallets;
    state.isLoading = true;
    try {
      const wallets = await api.loadWallets();
      for (const wallet of wallets) {
        const newWallet = new Wallet(wallet);
        this.state.account.addWallet(newWallet);
        if (newWallet.lastUsed) this.setActiveWallet(newWallet);
      }
      wallets.isLoading = false;
    } catch (error) {
      state.errorLoading = 'Error loading wallets'; // TODO: i18n
    }
  }

  @action async loadActiveWalletTransactions() {
    const state = this.state.wallets;
    if (!state.activeWallet === null) throw new Error('No active wallet');
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
    this.state.wallets.activeWallet = wallet;
    this.loadActiveWalletTransactions();
  }

  @action async sendMoney(transactionDetails: {
    receiver: string,
    amount: string,
    description: ?string
  }) {
    const wallets = this.state.wallets;
    try {
      const transaction = await api.sendMoney({
        ...transactionDetails,
        amount: parseFloat(transactionDetails.amount),
        sender: wallets.activeWallet.address,
        currency: wallets.activeWallet.currency
      });
      wallets.activeWallet.addTransaction(new WalletTransaction(transaction));
      if (this.state.router) this.state.router.transitionTo('/wallet/home');
    } catch (error) {
      wallets.errorSendingMoney = error;
    }
  }

  @action async createPersonalWallet(newWalletData: { name: string, currency: string}) {
    try {
      const createdWallet = await api.createPersonalWallet(newWalletData);
      const newWallet = new Wallet(createdWallet);
      this.state.account.addWallet(newWallet);
      this.setActiveWallet(newWallet);
      // TODO: Navigate to newly created wallet
    } catch (error) {
      this.state.wallets.errorCreating = error; // TODO: handling errors from backend and i18n
    }
  }

}
