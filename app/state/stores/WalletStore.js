// @flow
import { observable } from 'mobx';
import { Wallet } from '../../domain/Wallet';

export class WalletStore {

  @observable isLoadingWallets: boolean = true;
  @observable wallets: Array<Wallet> = [];
  @observable errorLoadingWallets: ?string;
  @observable errorCreatingAWallet: ?string;
  @observable errorLoadingWalletTransactions: ?string;
  @observable errorSendingMoney: ?string;

}
