// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import moment from 'moment';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import currencySettingsIcon from '../../../assets/images/currency-settings-ic.inline.svg';
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
  currencyFetched: {
    id: 'wallet.summary.page.currencyFetched',
    defaultMessage: '!!!Fetched {fetchedTimeAgo}',
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
      currencySelected,
      currencyRate,
      onCurrencySettingClick,
      // currencyFetched,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
    ]);

    const currencyFetched = moment().subtract(5, 'minutes');

    const isRestoreActive = wallet.isRestoring;
    const hasCurrency = !!currencySelected && !!currencyRate;

    let walletAmount = isRestoreActive
      ? '-'
      : formattedWalletAmount(wallet.amount, false);

    const currencyWalletAmount = hasCurrency
      ? formattedWalletCurrencyAmount(wallet.amount, currencyRate)
      : null;
    const currencyWalletAmountSymbol = currencySelected.symbol.toUpperCase();
    const fetchedTimeAgo = moment().fromNow();

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletContent}>
            <div>
              <div className={styles.walletName}>{wallet.name}</div>
              <div className={styles.walletAmount}>
                {walletAmount}
                <span className={styles.currencySymbol}>ADA</span>
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
                  Estimated conversion:
                </div>
                <div className={styles.currencyWalletAmount}>
                  {currencyWalletAmount}
                  <span className={styles.currencySymbol}>ADA</span>
                </div>
                <div className={styles.currencyRate}>
                  1 ADA = {currencyRate} {currencyWalletAmountSymbol}
                </div>
                <button
                  className={styles.currencyFetched}
                  onClick={onCurrencySettingClick}
                >
                  {intl.formatMessage(messages.currencyFetched, {
                    fetchedTimeAgo,
                  })}
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
