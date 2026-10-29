import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage, intlShape } from 'react-intl';
import classnames from 'classnames';
import { Button } from 'react-polymorph/lib/components/Button';
import type { Reward } from '../../../api/staking/types';
import globalMessages from '../../../i18n/global-messages';
import type LocalizableError from '../../../i18n/LocalizableError';
import BorderedBox from '../../widgets/BorderedBox';
import sendFormMessages from '../send-form/messages';
import { messages as heldRewardsMessages } from '../../notifications/HeldRewardsNotification';
import styles from './WalletSummaryHeader.scss';
import Wallet from '../../../domains/Wallet';
import { formattedWalletAmount } from '../../../utils/formatters';
import { DiscreetValue } from '../../../features/discreet-mode';
import WalletSummaryHeaderRewards from './WalletSummaryHeaderRewards';

const messages = defineMessages({
  withdrawRewards: {
    id: 'wallet.summary.header.withdrawRewards',
    defaultMessage: '!!!Withdraw rewards',
    description:
      "Button beside the wallet's rewards that opens a transaction withdrawing them",
  },
  transactionsLabel: {
    id: 'wallet.summary.header.transactionsLabel',
    defaultMessage: '!!!{total} transactions, {pending} pending',
    description: '"Number of transactions" label on Wallet summary header page',
  },
});
// The action that withdraws the wallet's staking rewards; null hides it.
export type RewardsAction = {
  disabled: boolean;
  reason?: 'ledger_not_delegated';
  isSubmitting: boolean;
  error: LocalizableError | null;
  onWithdraw: () => void;
  onDelegate: () => void;
};

type Props = {
  rewardsAction?: RewardsAction | null;
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
      rewardsAction,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
    ]);
    const walletNameStyles = classnames([styles.walletName]);
    const walletAmountStyles = classnames([styles.walletAmount]);
    const isRestoreActive = wallet.isRestoring;
    const walletAmount = isRestoreActive
      ? '–'
      : formattedWalletAmount(wallet.amount, false);
    const totalTransactions = isRestoreActive
      ? '–'
      : (numberOfTransactions ?? numberOfRecentTransactions);
    const pendingTransactions = isRestoreActive
      ? '–'
      : numberOfPendingTransactions;
    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletContent}>
            <div>
              <div className={walletNameStyles}>{wallet.name}</div>
              <div className={walletAmountStyles}>
                {/* @ts-ignore ts-migrate(2741) FIXME: Property 'replacer' is missing in type '{ children... Remove this comment to see the full error message */}
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
              {!wallet.isLegacy && rewardsAction && (
                <div className={styles.rewardsAction}>
                  <Button
                    className="flat"
                    label={intl.formatMessage(messages.withdrawRewards)}
                    disabled={
                      rewardsAction.disabled ||
                      rewardsAction.isSubmitting ||
                      isRestoreActive
                    }
                    onClick={rewardsAction.onWithdraw}
                  />
                  {rewardsAction.reason === 'ledger_not_delegated' && (
                    <p className={styles.rewardsActionReason} role="note">
                      {intl.formatMessage(
                        sendFormMessages.withdrawRewardsNotDelegated
                      )}
                      <Button
                        className="flat"
                        label={intl.formatMessage(
                          heldRewardsMessages.delegateButton
                        )}
                        onClick={rewardsAction.onDelegate}
                      />
                    </p>
                  )}
                  {rewardsAction.error && (
                    <p className={styles.rewardsActionError} role="alert">
                      <FormattedHTMLMessage {...rewardsAction.error} />
                    </p>
                  )}
                </div>
              )}
              {!isLoadingTransactions && (
                <div className={styles.transactionsCountWrapper}>
                  <div className={numberOfTransactionsStyles}>
                    <FormattedHTMLMessage
                      {...messages.transactionsLabel}
                      tagName="p"
                      values={{
                        total: totalTransactions,
                        pending: pendingTransactions,
                      }}
                    />
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
