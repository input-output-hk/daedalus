// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import adaSymbol from '../../../assets/images/ada-symbol.inline.svg';
import adaSymbolBig from '../../../assets/images/ada-symbol-big-dark.inline.svg';
import BorderedBox from '../../widgets/BorderedBox';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import styles from './WalletSummary.scss';
import Wallet from '../../../domains/Wallet';

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
  walletBalancelabel: {
    id: 'wallet.summary.page.walletBalancelabel',
    defaultMessage: '!!!Total balance',
    description: '"Wallet amount" label on Wallet summary page',
  },
  walletAvailableBalanceLabel: {
    id: 'wallet.summary.page.walletAvailableBalanceLabel',
    defaultMessage: '!!!Available balance',
    description: '"Wallet amount" label on Wallet summary page',
  },
  rewardsAccountBalancelabel: {
    id: 'wallet.summary.page.rewardsAccountBalancelabel',
    defaultMessage: '!!!Rewards balance',
    description: '"Rewards account balance" label on Wallet summary page',
  },
  rewardsSpendingNote: {
    id: 'wallet.summary.page.rewardsSpendingNote',
    defaultMessage:
      '!!!Temporary Daedalus limitations prevent reward spending.',
    description: '"Rewards account balance" spending note',
  },
});

type Props = {
  wallet: Wallet,
  numberOfRecentTransactions: number,
  numberOfTransactions?: number,
  numberOfPendingTransactions: number,
  isLoadingTransactions: boolean,
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
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
    ]);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletName}>{wallet.name}</div>
          <div className={styles.walletAmount}>
            {wallet.availableAmount.toFormat(DECIMAL_PLACES_IN_ADA)}
            <SVGInline
              svg={adaSymbolBig}
              className={styles.currencySymbolBig}
            />
          </div>

          {!wallet.isLegacy && (
            <div className={styles.balancesWrapper}>
              <div className={styles.walletBalance}>
                {intl.formatMessage(messages.walletBalancelabel)}:&nbsp;
                <span>{wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA)}</span>
                <SVGInline svg={adaSymbol} className={styles.currencySymbol} />
              </div>
              <div className={styles.walletAvailableBalance}>
                {intl.formatMessage(messages.walletAvailableBalanceLabel)}
                :&nbsp;
                <span>
                  {wallet.availableAmount.toFormat(DECIMAL_PLACES_IN_ADA)}
                </span>
                <SVGInline svg={adaSymbol} className={styles.currencySymbol} />
              </div>
              <div className={styles.rewardsAccountBalance}>
                {intl.formatMessage(messages.rewardsAccountBalancelabel)}:&nbsp;
                <span>{wallet.reward.toFormat(DECIMAL_PLACES_IN_ADA)}</span>
                <SVGInline svg={adaSymbol} className={styles.currencySymbol} />
              </div>
            </div>
          )}

          {!isLoadingTransactions ? (
            <div className={styles.transactionsCountWrapper}>
              <div className={styles.numberOfPendingTransactions}>
                {intl.formatMessage(messages.pendingTransactionsLabel)}:&nbsp;
                <span>{numberOfPendingTransactions}</span>
              </div>
              <div className={numberOfTransactionsStyles}>
                {intl.formatMessage(messages.transactionsLabel)}:&nbsp;
                <span>
                  {numberOfTransactions || numberOfRecentTransactions}
                </span>
              </div>
            </div>
          ) : null}
          <p className={styles.rewardsSpendingNote}>
            {intl.formatMessage(messages.rewardsSpendingNote)}
          </p>
        </BorderedBox>
      </div>
    );
  }
}
