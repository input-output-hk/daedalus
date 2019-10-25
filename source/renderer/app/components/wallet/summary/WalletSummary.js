// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import adaSymbolBig from '../../../assets/images/ada-symbol-big-dark.inline.svg';
import adaSymbolSmallest from '../../../assets/images/ada-symbol-smallest-dark.inline.svg';
import BorderedBox from '../../widgets/BorderedBox';
import { DECIMAL_PLACES_IN_ADA } from '../../../config/numbersConfig';
import type { UnconfirmedAmount } from '../../../types/unconfirmedAmountType';
import styles from './WalletSummary.scss';
import Wallet from '../../../domains/Wallet';

const messages = defineMessages({
  pendingOutgoingConfirmationLabel: {
    id: 'wallet.summary.page.pendingOutgoingConfirmationLabel',
    defaultMessage: '!!!Outgoing pending confirmation',
    description: '"Outgoing pending confirmation" label on Wallet summary page',
  },
  pendingIncomingConfirmationLabel: {
    id: 'wallet.summary.page.pendingIncomingConfirmationLabel',
    defaultMessage: '!!!Incoming pending confirmation',
    description: '"Incoming pending confirmation" label on Wallet summary page',
  },
  transactionsLabel: {
    id: 'wallet.summary.page.transactionsLabel',
    defaultMessage: '!!!Number of transactions',
    description: '"Number of transactions" label on Wallet summary page',
  },
});

type Props = {
  wallet: Wallet,
  numberOfRecentTransactions: number,
  numberOfTransactions?: number,
  pendingAmount: UnconfirmedAmount,
  isLoadingTransactions: boolean,
  isRestoreActive: boolean,
};

@observer
export default class WalletSummary extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      wallet,
      pendingAmount,
      numberOfRecentTransactions,
      numberOfTransactions,
      isLoadingTransactions,
      isRestoreActive,
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
            {wallet.amount.toFormat(DECIMAL_PLACES_IN_ADA)}
            <SVGInline
              svg={adaSymbolBig}
              className={styles.currencySymbolBig}
            />
          </div>

          {!isRestoreActive ? (
            <div>
              {pendingAmount.incoming.greaterThan(0) && (
                <div className={styles.pendingConfirmation}>
                  {`${intl.formatMessage(
                    messages.pendingIncomingConfirmationLabel
                  )}`}
                  : {pendingAmount.incoming.toFormat(DECIMAL_PLACES_IN_ADA)}
                  <SVGInline
                    svg={adaSymbolSmallest}
                    className={styles.currencySymbolSmallest}
                  />
                </div>
              )}
              {pendingAmount.outgoing.greaterThan(0) && (
                <div className={styles.pendingConfirmation}>
                  {`${intl.formatMessage(
                    messages.pendingOutgoingConfirmationLabel
                  )}`}
                  : {pendingAmount.outgoing.toFormat(DECIMAL_PLACES_IN_ADA)}
                  <SVGInline
                    svg={adaSymbolSmallest}
                    className={styles.currencySymbolSmallest}
                  />
                </div>
              )}
            </div>
          ) : null}

          {!isLoadingTransactions ? (
            <div className={numberOfTransactionsStyles}>
              {intl.formatMessage(messages.transactionsLabel)}:&nbsp;
              {numberOfTransactions || numberOfRecentTransactions}
            </div>
          ) : null}
        </BorderedBox>
      </div>
    );
  }
}
