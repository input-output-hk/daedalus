// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import currencySettingsIcon from '../../../assets/images/currency-settings-ic.inline.svg';
import globalMessages from '../../../i18n/global-messages';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummary.scss';
import Wallet from '../../../domains/Wallet';
import {
  formattedWalletAmount,
  formattedWalletCurrencyAmount,
} from '../../../utils/formatters';
import type { Currency } from '../../../types/currencyTypes';

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
  currencyTitle: {
    id: 'wallet.summary.page.currency.title',
    defaultMessage: '!!!Converts as',
    description: '"Currency - title" label on Wallet summary page',
  },
  currencyLastFetched: {
    id: 'wallet.summary.page.currency.lastFetched',
    defaultMessage: '!!!converted {fetchedTimeAgo}',
    description: '"Currency - last fetched" label on Wallet summary page',
  },
  currencyIsFetchingRate: {
    id: 'wallet.summary.page.currency.isFetchingRate',
    defaultMessage: '!!!fetching conversion rates',
    description: '"Currency - Fetching" label on Wallet summary page',
  },
});

type Props = {
  wallet: Wallet,
  numberOfRecentTransactions: number,
  numberOfTransactions?: number,
  numberOfPendingTransactions: number,
  isLoadingTransactions: boolean,
  currencyIsFetchingRate: boolean,
  currencyIsAvailable: boolean,
  currencyIsActive: boolean,
  currencySelected: ?Currency,
  currencyRate: ?number,
  currencyLastFetched: ?Date,
  onCurrencySettingClick: Function,
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
      currencyIsActive,
      currencyIsAvailable,
      currencyIsFetchingRate,
      currencyLastFetched,
      currencyRate,
      currencySelected,
      onCurrencySettingClick,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
    ]);

    const isRestoreActive = wallet.isRestoring;
    const hasCurrency =
      currencyIsActive &&
      currencyIsAvailable &&
      !!currencySelected &&
      (!!currencyRate || currencyIsFetchingRate);

    const walletAmount = isRestoreActive
      ? '-'
      : formattedWalletAmount(wallet.amount, false);

    const { decimalDigits } = currencySelected || {};

    let currencyWalletAmount;
    if (isRestoreActive) currencyWalletAmount = '- ';
    else if (hasCurrency && currencyRate)
      currencyWalletAmount = formattedWalletCurrencyAmount(
        wallet.amount,
        currencyRate,
        decimalDigits
      );
    const currencyWalletAmountSymbol = currencySelected
      ? currencySelected.symbol.toUpperCase()
      : '';
    const fetchedTimeAgo = moment(currencyLastFetched)
      .locale(intl.locale)
      .fromNow();

    const buttonClasses = classnames([
      styles.currencyLastFetched,
      currencyIsFetchingRate ? styles.currencyIsFetchingRate : null,
    ]);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletContent}>
            <div>
              <div className={styles.walletName}>{wallet.name}</div>
              <div className={styles.walletAmount}>
                {walletAmount}
                <span className={styles.currencySymbol}>
                  {intl.formatMessage(globalMessages.unitAda)}
                </span>
              </div>
              {!isLoadingTransactions ? (
                <div className={styles.transactionsCountWrapper}>
                  <div className={styles.numberOfPendingTransactions}>
                    {intl.formatMessage(messages.pendingTransactionsLabel)}
                    :&nbsp;
                    {numberOfPendingTransactions}
                  </div>
                  <div className={numberOfTransactionsStyles}>
                    {intl.formatMessage(messages.transactionsLabel)}:&nbsp;
                    {numberOfTransactions || numberOfRecentTransactions}
                  </div>
                </div>
              ) : null}
            </div>

            {hasCurrency && (
              <div className={styles.currency}>
                <div className={styles.currencyTitle}>
                  {intl.formatMessage(messages.currencyTitle)}
                </div>
                <div className={styles.currencyWalletAmount}>
                  {currencyWalletAmount}
                  <span className={styles.currencySymbol}>
                    {currencyWalletAmountSymbol}
                  </span>
                </div>
                <div className={styles.currencyRate}>
                  1 {intl.formatMessage(globalMessages.unitAda)} ={' '}
                  {currencyRate} {currencyWalletAmountSymbol}
                </div>
                <button
                  className={buttonClasses}
                  onClick={onCurrencySettingClick}
                >
                  <em>
                    {currencyIsFetchingRate
                      ? intl.formatMessage(messages.currencyIsFetchingRate)
                      : intl.formatMessage(messages.currencyLastFetched, {
                          fetchedTimeAgo,
                        })}
                  </em>
                  <SVGInline
                    svg={currencySettingsIcon}
                    className={styles.currencySettingsIcon}
                  />
                </button>
              </div>
            )}
          </div>
        </BorderedBox>
      </div>
    );
  }
}
