// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import numeral from 'numeral';
import adaSymbolBig from '../../../assets/images/ada-symbol-big-dark.svg';
import adaSymbolSmallest from '../../../assets/images/ada-symbol-smallest-dark.svg';
import BorderedBox from '../../widgets/BorderedBox';
import styles from './WalletSummary.scss';

const messages = defineMessages({
  pendingConfirmationLabel: {
    id: 'wallet.summary.page.pendingConfirmationLabel',
    defaultMessage: '!!!Pending confirmation',
    description: '"Pending confirmation" label on Wallet summary page'
  },
  transactionsLabel: {
    id: 'wallet.summary.page.transactionsLabel',
    defaultMessage: '!!!Number of transactions',
    description: '"Number of transactions" label on Wallet summary page'
  }
});

@observer
export default class WalletSummary extends Component {

  static propTypes = {
    walletName: PropTypes.string.isRequired,
    amount: PropTypes.number.isRequired,
    numberOfTransactions: PropTypes.number.isRequired,
    pendingAmount: PropTypes.number.isRequired,
    isLoadingTransactions: PropTypes.bool.isRequired,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const {
      walletName,
      amount,
      pendingAmount,
      numberOfTransactions,
      isLoadingTransactions
    } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>
        <BorderedBox>
          <div className={styles.walletName}>{walletName}</div>
          <div className={styles.walletAmount}>
            {numeral(amount).format('0,0')}
            <img src={adaSymbolBig} role="presentation" />
          </div>
          {pendingAmount > 0 && (
            <div className={styles.pendingConfirmation}>
              {`${intl.formatMessage(messages.pendingConfirmationLabel)}`}: {pendingAmount}
              <img src={adaSymbolSmallest} role="presentation" />
            </div>
          )}
          {!isLoadingTransactions && (
            <div className={styles.numberOfTransactions}>
              {intl.formatMessage(messages.transactionsLabel)}: {numberOfTransactions}
            </div>
          )}
        </BorderedBox>
      </div>
    );
  }

}
