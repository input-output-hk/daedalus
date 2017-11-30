// @flow
import { computed } from 'mobx';
import BigNumber from 'bignumber.js';
import type { UnconfirmedAmount } from '../../types/unconfirmedAmountType';
import { isValidAmountInLovelaces } from '../../utils/validations';
import TransactionsStore from '../TransactionsStore';
import { transactionTypes } from '../../domain/WalletTransaction';

export default class AdaTransactionsStore extends TransactionsStore {

  @computed get unconfirmedAmount(): UnconfirmedAmount {
    const unconfirmedAmount = {
      total: new BigNumber(0),
      incoming: new BigNumber(0),
      outgoing: new BigNumber(0),
    };
    const wallet = this.stores.ada.wallets.active;
    if (!wallet) return unconfirmedAmount;
    const result = this._getTransactionsAllRequest(wallet.id).result;
    if (!result || !result.transactions) return unconfirmedAmount;

    for (const transaction of result.transactions) {
      // TODO: move this magic constant (required numberOfConfirmations) to config!
      if (transaction.numberOfConfirmations <= 6) {
        unconfirmedAmount.total = unconfirmedAmount.total.plus(transaction.amount.absoluteValue());
        if (transaction.type === transactionTypes.EXPEND) {
          unconfirmedAmount.outgoing = unconfirmedAmount.outgoing.plus(
            transaction.amount.absoluteValue()
          );
        }
        if (transaction.type === transactionTypes.INCOME) {
          unconfirmedAmount.incoming = unconfirmedAmount.incoming.plus(
            transaction.amount.absoluteValue()
          );
        }
      }
    }
    return unconfirmedAmount;
  }

  calculateTransactionFee = (walletId: string, receiver: string, amount: string) => {
    const accountId = this.stores.ada.addresses._getAccountIdByWalletId(walletId);
    if (!accountId) throw new Error('Active account required before calculating transaction fees.');
    return this.api.ada.calculateTransactionFee({ sender: accountId, receiver, amount });
  };

  validateAmount = (amountInLovelaces: string): Promise<boolean> => (
    Promise.resolve(isValidAmountInLovelaces(amountInLovelaces))
  );

}
