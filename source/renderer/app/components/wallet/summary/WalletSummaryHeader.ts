import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import classnames from 'classnames';
import type { Reward } from '../../../api/staking/types';
import globalMessages from '../../../i18n/global-messages';
import BorderedBox from '../../widgets/BorderedBox';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletSummaryHeader.scss' or... Remove this comment to see the full error message
import styles from './WalletSummaryHeader.scss';
import Wallet from '../../../domains/Wallet';
import { formattedWalletAmount } from '../../../utils/formatters';
import { DiscreetValue } from '../../../features/discreet-mode';
import WalletSummaryHeaderRewards from './WalletSummaryHeaderRewards';

const messages = defineMessages({
  transactionsLabel: {
    id: 'wallet.summary.header.transactionsLabel',
    defaultMessage: '!!!Number of transactions',
    description: '"Number of transactions" label on Wallet summary header page',
  },
  pendingTransactionsLabel: {
    id: 'wallet.summary.header.pendingTransactionsLabel',
    defaultMessage: '!!!Number of pending transactions',
    description:
      '"Number of pending transactions" label on Wallet summary header page',
  },
});
type Props = {
  wallet: Wallet;
  reward: Reward;
  numberOfRecentTransactions: number;
  numberOfTransactions?: number;
  numberOfPendingTransactions: number;
  isLoadingTransactions: boolean;
  currency?: Node;
};

@observer
class WalletSummaryHeader extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      wallet,
      reward,
      numberOfPendingTransactions,
      numberOfRecentTransactions,
      numberOfTransactions,
      isLoadingTransactions,
      currency,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
    ]);
    const numberOfPendingTransactionsStyles = classnames([
      styles.numberOfPendingTransactions,
    ]);
    const walletNameStyles = classnames([styles.walletName]);
    const walletAmountStyles = classnames([styles.walletAmount]);
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
                <DiscreetValue>{walletAmount}</DiscreetValue>
                <span className={styles.currencyCode}>
                  {intl.formatMessage(globalMessages.adaUnit)}
                </span>
              </div>
              {!wallet.isLegacy && (
                <div className={styles.rewards}>
                  <WalletSummaryHeaderRewards
                    isRestoring={isRestoreActive}
                    total={reward.total}
                    unspent={reward.unspent}
                    walletAmount={wallet.amount}
                  />
                </div>
              )}
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
            <div className={styles.currency}>{currency}</div>
          </div>
        </BorderedBox>
      </div>
    );
  }
}

export default WalletSummaryHeader;
