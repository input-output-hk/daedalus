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
import globalMessages from '../../../i18n/global-messages';

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
  hasNativeTokens?: boolean,
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
      hasNativeTokens,
    } = this.props;
    const { intl } = this.context;
    const isLoadingAllTransactions =
      numberOfRecentTransactions && !numberOfTransactions;
    const numberOfTransactionsStyles = classnames([
      styles.numberOfTransactions,
      isLoadingAllTransactions ? styles.isLoadingNumberOfTransactions : null,
      hasNativeTokens ? styles.nativeTokens : null,
    ]);

    const numberOfPendingTransactionsStyles = classnames([
      styles.numberOfPendingTransactions,
      hasNativeTokens ? styles.nativeTokens : null,
    ]);

    const walletNameStyles = classnames([
      styles.walletName,
      hasNativeTokens ? styles.nativeTokens : null,
    ]);

    const walletAmountStyles = classnames([
      styles.walletAmount,
      hasNativeTokens ? styles.nativeTokens : null,
    ]);

    const isRestoreActive = wallet.isRestoring;

    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={walletNameStyles}>{wallet.name}</div>
          <div className={walletAmountStyles}>
            {isRestoreActive
              ? '-'
              : wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
            {hasNativeTokens ? (
              <span>&nbsp;{intl.formatMessage(globalMessages.unitAda)}</span>
            ) : (
              <SVGInline
                svg={adaSymbolBig}
                className={styles.currencySymbolBig}
              />
            )}
          </div>

          {!isLoadingTransactions ? (
            <div className={styles.transactionsCountWrapper}>
              <div className={numberOfPendingTransactionsStyles}>
                <span>
                  {intl.formatMessage(messages.pendingTransactionsLabel)}
                </span>
                :&nbsp;
                <span>{numberOfPendingTransactions}</span>
              </div>
              <div className={numberOfTransactionsStyles}>
                <span>{intl.formatMessage(messages.transactionsLabel)}</span>
                :&nbsp;
                <span>
                  {numberOfTransactions || numberOfRecentTransactions}
                </span>
              </div>
            </div>
          ) : null}
        </BorderedBox>
      </div>
    );
  }
}
