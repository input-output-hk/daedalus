import { action } from 'mobx';
import _ from 'lodash';
import Wallet from '../domain/Wallet';
import WalletTransaction from '../domain/WalletTransaction';
import { INITIAL_WALLET_SEARCH_LIMIT } from '../state/active-wallet';
import BaseController from './BaseController';

export default class WalletsController extends BaseController {

  @action async loadWallets() {
    const { activeWallet, user } = this.state;
    activeWallet.isLoading = true;
    try {
      const wallets = await this.api.getWallets(user.id);
      for (const wallet of wallets) {
        const newWallet = new Wallet(wallet);
        user.addWallet(newWallet);
        if (newWallet.lastUsed) this.setActiveWallet(newWallet);
      }
      activeWallet.isLoading = false;
    } catch (error) {
      throw error;
    }
  }

  @action async loadActiveWalletTransactions(initialLoading: boolean) {
    const { activeWallet } = this.state;
    const { wallet } = activeWallet;
    if (!wallet === null) throw new Error('No active wallet');
    activeWallet.isLoadingTransactions = true;
    if (initialLoading) activeWallet.hasAnyTransactions = false;
    const result = await this.api.getTransactions({
      walletId: wallet.id,
      searchTerm: activeWallet.transactionsSearchTerm,
      limit: activeWallet.transactionsSearchLimit
    });
    wallet.transactions.clear();
    for (const transaction of result.transactions) {
      wallet.addTransaction(new WalletTransaction(transaction));
    }
    if (initialLoading) {
      activeWallet.hasAnyTransactions = result.total > 0;
      activeWallet.initialLoading = false;
    }
    activeWallet.totalAvailableTransactions = result.total;
    activeWallet.isLoadingTransactions = false;
  }

  @action setActiveWallet(wallet: string|Wallet) {
    let newActiveWallet = wallet;
    if (_.isString(newActiveWallet)) {
      newActiveWallet = _.find(this.state.user.wallets, { id: newActiveWallet });
    }
    console.log('new active wallet', newActiveWallet);
    // TODO: bugfix sometimes new active wallet is undefined
    const activeWallet = this.state.activeWallet;
    if (newActiveWallet === activeWallet.wallet) return;
    activeWallet.wallet = newActiveWallet;
    activeWallet.transactionsSearchLimit = INITIAL_WALLET_SEARCH_LIMIT;
    activeWallet.transactionsSearchTerm = '';
    this.loadActiveWalletTransactions(true);
    this.appController.navigateTo(`/wallet/${newActiveWallet.id}/home`);
  }

  @action async sendMoney(transactionDetails: {
    receiver: string,
    amount: string,
    description: ?string
  }) {
    const { activeWallet } = this.state;
    const { wallet } = activeWallet;
    try {
      const transaction = await this.api.createTransaction({
        ...transactionDetails,
        walletId: wallet.id,
        amount: parseFloat(transactionDetails.amount),
        sender: wallet.address,
        currency: wallet.currency
      });
      wallet.addTransaction(new WalletTransaction(transaction));
      this.appController.navigateTo(`/wallet/${wallet.address}/home`);
      this.loadActiveWalletTransactions();
    } catch (error) {
      throw error;
    }
  }

  @action async createPersonalWallet(newWalletData: { name: string, currency: string}) {
    try {
      const createdWallet = await this.api.createWallet(newWalletData);
      const newWallet = new Wallet(createdWallet);
      this.state.user.addWallet(newWallet);
      this.setActiveWallet(newWallet);
      // TODO: Navigate to newly created wallet
    } catch (error) {
      throw error;
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

  @action toggleCreateWalletDialog() {
    this.state.isCreateWalletDialogOpen = !this.state.isCreateWalletDialogOpen;
  }
}
