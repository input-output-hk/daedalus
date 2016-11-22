import { action } from 'mobx';
import _ from 'lodash';
import type { appState } from '../state/index';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import api from '../api';
import { INITIAL_WALLET_SEARCH_LIMIT } from '../state/active-wallet';

export default class WalletsController {

  state: appState;

  constructor(state: appState) {
    this.state = state;
  }

  @action async loadWallets() {
    const { activeWallet, user } = this.state;
    activeWallet.isLoading = true;
    try {
      const wallets = await api.loadWallets();
      for (const wallet of wallets) {
        const newWallet = new Wallet(wallet);
        user.addWallet(newWallet);
        if (newWallet.lastUsed) this.setActiveWallet(newWallet);
      }
      activeWallet.isLoading = false;
    } catch (error) {
      activeWallet.errorLoading = 'Error loading wallets'; // TODO: i18n
    }
  }

  @action async loadActiveWalletTransactions(initialLoading: boolean) {
    const { activeWallet } = this.state;
    const { wallet } = activeWallet;
    if (!wallet === null) throw new Error('No active wallet');
    activeWallet.isLoadingTransactions = true;
    if (initialLoading) activeWallet.hasAnyTransactions = false;
    const result = await api.loadWalletTransactions({
      address: wallet.address,
      searchTerm: activeWallet.transactionsSearchTerm,
      limit: activeWallet.transactionsSearchLimit
    });
    wallet.transactions.clear();
    for (const transaction of result.transactions) {
      wallet.addTransaction(new WalletTransaction(transaction));
    }
    activeWallet.totalAvailableTransactions = result.total;
    activeWallet.isLoadingTransactions = false;
    if (initialLoading) activeWallet.hasAnyTransactions = result.total > 0;
  }

  @action setActiveWallet(walletId: string|Wallet) {
    let wallet = walletId;
    if (_.isString(walletId)) {
      wallet = _.find(this.state.user.wallets, { address: wallet });
    }
    const activeWallet = this.state.activeWallet;
    if (wallet === activeWallet.wallet) return;
    activeWallet.wallet = wallet;
    activeWallet.transactionsSearchLimit = INITIAL_WALLET_SEARCH_LIMIT;
    activeWallet.transactionsSearchTerm = '';
    this.loadActiveWalletTransactions(true);
    if (this.state.router) this.state.router.transitionTo(`/wallet/${wallet.address}/home`);
  }

  @action async sendMoney(transactionDetails: {
    receiver: string,
    amount: string,
    description: ?string
  }) {
    const { activeWallet } = this.state;
    const { wallet } = activeWallet;
    try {
      const transaction = await api.sendMoney({
        ...transactionDetails,
        amount: parseFloat(transactionDetails.amount),
        sender: wallet.address,
        currency: wallet.currency
      });
      wallet.addTransaction(new WalletTransaction(transaction));
      if (this.state.router) this.state.router.transitionTo(`/wallet/${wallet.address}/home`);
    } catch (error) {
      activeWallet.errorSendingMoney = error;
    }
  }

  @action async createPersonalWallet(newWalletData: { name: string, currency: string}) {
    try {
      const createdWallet = await api.createPersonalWallet(newWalletData);
      const newWallet = new Wallet(createdWallet);
      this.state.user.addWallet(newWallet);
      this.setActiveWallet(newWallet);
      // TODO: Navigate to newly created wallet
    } catch (error) {
      this.state.activeWallet.errorCreating = error; // TODO: handling errors from backend and i18n
    }
  }

  @action filterTransactions(searchTerm) {
    const { activeWallet } = this.state;
    activeWallet.transactionsSearchTerm = searchTerm;
    activeWallet.wallet.transactions.clear();
    this.loadActiveWalletTransactions();
  }

  @action loadMoreTransactions() {
    this.state.activeWallet.transactionsSearchLimit += 10;
    this.loadActiveWalletTransactions();
  }
}
