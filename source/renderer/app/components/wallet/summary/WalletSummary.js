// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import adaSymbolBig from '../../../assets/images/ada-symbol-big-dark.inline.svg';
import BorderedBox from '../../widgets/BorderedBox';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './WalletSummary.scss';
import Wallet from '../../../domains/Wallet';
import {
  formattedWalletAmount,
  formattedWalletCurrencyAmount,
} from '../../../utils/formatters';

const messages = defineMessages({
  transactionsLabel: {
    id: 'wallet.summary.page.transactionsLabel',
    defaultMessage: '!!!Number of transactions',
    description: '"Number of transactions" label on Wallet summary page',
  },
  pendingTransactionsLabel: {
    id: 'wallet.summary.page.pendingTransactionsLabel',
    defaultMessage: '!!!Number of pending transactions',
    description:
      '"Number of pending transactions" label on Wallet summary page',
  },
});

type Props = {
  wallet: Wallet,
  numberOfRecentTransactions: number,
  numberOfTransactions?: number,
  numberOfPendingTransactions: number,
  isLoadingTransactions: boolean,
  currencySelected: ?Currency,
  currencyRate: ?number,
};

@observer
export default class WalletSummary extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      wallet,
      numberOfPendingTransactions,
      numberOfRecentTransactions,
      numberOfTransactions,
      isLoadingTransactions,
      currencySelected,
      currencyRate,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
    ]);

    const isRestoreActive = wallet.isRestoring;
    const hasCurrency = !!currencySelected && !!currencyRate;

    let walletAmount = '-';
    if (!isRestoreActive) {
      if (hasCurrency) {
        walletAmount = formattedWalletCurrencyAmount(
          wallet.amount,
          currencyRate
        );
      } else {
        walletAmount = formattedWalletAmount(wallet.amount, false);
      }
    }
    const walletAmountSymbol = hasCurrency ? (
      currencySelected.symbol.toUpperCase()
    ) : (
      <SVGInline svg={adaSymbolBig} className={styles.currencySymbolBig} />
    );

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletName}>{wallet.name}</div>
          <div className={styles.walletAmount}>
            {walletAmount}
            <span className={styles.walletAmountSymbol}>
              {walletAmountSymbol}
            </span>
          </div>

          {!isLoadingTransactions ? (
            <div className={styles.transactionsCountWrapper}>
              <div className={styles.numberOfPendingTransactions}>
                {intl.formatMessage(messages.pendingTransactionsLabel)}:&nbsp;
                {numberOfPendingTransactions}
              </div>
              <div className={numberOfTransactionsStyles}>
                {intl.formatMessage(messages.transactionsLabel)}:&nbsp;
                {numberOfTransactions || numberOfRecentTransactions}
              </div>
            </div>
          ) : null}
        </BorderedBox>
      </div>
    );
  }
}
