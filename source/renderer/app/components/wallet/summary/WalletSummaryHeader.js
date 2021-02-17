// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import globalMessages from '../../../i18n/global-messages';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummaryHeader.scss';
import Wallet from '../../../domains/Wallet';
import { formattedWalletAmount } from '../../../utils/formatters';

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
  hasAssetsEnabled?: boolean,
  currency?: Node,
};

@observer
export default class WalletSummaryHeader extends Component<Props> {
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
      hasAssetsEnabled,
      currency,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
      hasAssetsEnabled ? styles.assets : null,
    ]);

    const numberOfPendingTransactionsStyles = classnames([
      styles.numberOfPendingTransactions,
      hasAssetsEnabled ? styles.assets : null,
    ]);

    const walletNameStyles = classnames([
      styles.walletName,
      hasAssetsEnabled ? styles.assets : null,
    ]);

    const walletAmountStyles = classnames([
      styles.walletAmount,
      hasAssetsEnabled ? styles.assets : null,
    ]);

    const isRestoreActive = wallet.isRestoring;

    const walletAmount = isRestoreActive
      ? '-'
      : formattedWalletAmount(wallet.amount, false);

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletContent}>
            <div>
              <div className={walletNameStyles}>{wallet.name}</div>
              <div className={walletAmountStyles}>
                {walletAmount}
                {hasAssetsEnabled ? (
                  <span>
                    &nbsp;{intl.formatMessage(globalMessages.unitAda)}
                  </span>
                ) : (
                  <span className={styles.currencySymbol}>
                    {intl.formatMessage(globalMessages.unitAda)}
                  </span>
                )}
              </div>
              {!isLoadingTransactions && (
                <div className={styles.transactionsCountWrapper}>
                  <div className={numberOfPendingTransactionsStyles}>
                    <span>
                      {intl.formatMessage(messages.pendingTransactionsLabel)}
                    </span>
                    :&nbsp;
                    <span>{numberOfPendingTransactions}</span>
                  </div>
                  <div className={numberOfTransactionsStyles}>
                    <span>
                      {intl.formatMessage(messages.transactionsLabel)}
                    </span>
                    :&nbsp;
                    <span>
                      {numberOfTransactions || numberOfRecentTransactions}
                    </span>
                  </div>
                </div>
              )}
            </div>
            {currency}
          </div>
        </BorderedBox>
      </div>
    );
  }
}
