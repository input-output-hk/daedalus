// @flow
import { observable, action } from 'mobx';
import { UiStore } from './UiStore';
import { Wallet } from '../domain/Wallet';
import { WalletTransaction } from '../domain/WalletTransaction';
import {
  loadWallets,
  createPersonalWallet,
  loadWalletTransactions,
  sendMoney
} from '../../api-stub';

export class WalletStore {

  uiStore: UiStore;
  @observable isLoading: boolean = true;
  @observable wallets: Array<Wallet> = [];
  @observable errorLoadingWallets: ?string;
  @observable errorCreatingAWallet: ?string;
  @observable errorLoadingWalletTransactions: ?string;
  @observable errorSendingMoney: ?string;

  constructor(uiStore: UiStore) {
    this.uiStore = uiStore;
    this.loadWallets();
  }

  @action loadWallets() {
    this.isLoading = true;
    loadWallets((error, data) => {
      if (error) {
        this.errorLoadingWallets = 'Error loading wallets'; // TODO: i18n
      } else {
        for (const wallet of data) {
          const newWallet = new Wallet(wallet);
          this.wallets.push(newWallet);
          if (newWallet.lastUsed) {
            this.uiStore.selectWallet(newWallet);
            this.loadWalletTransactions(newWallet);
          }
        }
        this.isLoading = false;
      }
    });
  }

  @action loadWalletTransactions(wallet: Wallet) {
    loadWalletTransactions({ address: wallet.address }, (error, transactions) => {
      if (error) {
        this.errorLoadingWalletTransactions = error; // TODO: handling errors from backend and i18n
      } else {
        for (const transaction of transactions) {
          const newTransaction = new WalletTransaction(transaction);
          wallet.addTransaction(newTransaction);
        }
      }
    });
  }

  @action createPersonalWallet(name: string, currency: string) {
    createPersonalWallet({ name, currency }, (error, createdWallet) => {
      if (error) {
        this.errorCreatingAWallet = error; // TODO: handling errors from backend and i18n
      } else {
        const newWallet = new Wallet(createdWallet);
        this.wallets.push(newWallet);
        this.uiStore.selectWallet(newWallet);
        this.loadWalletTransactions(newWallet);
        // TODO: Navigate to newly created wallet
      }
    });
  }

  @action sendMoney(fromWallet: Wallet, transactionDetails: {
    receiver: string,
    amount: string,
    currency: string,
    description: ?string
  }) {
    sendMoney({
      ...transactionDetails,
      amount: parseFloat(transactionDetails.amount),
      sender: fromWallet.address
    }, (error, transaction) => {
      if (error) {
        this.errorSendingMoney = error;
      } else {
        fromWallet.addTransaction(new WalletTransaction(transaction));
        this.uiStore.router.transitionTo('/wallet/home');
      }
    });
  }

}
