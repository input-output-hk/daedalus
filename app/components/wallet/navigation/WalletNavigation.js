// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletNavigation.scss';
import WalletNavButton from './WalletNavButton';
import summaryIcon from '../../../assets/images/wallet-nav/summary-ic-light.svg';
import summaryIconActive from '../../../assets/images/wallet-nav/summary-ic-dark.svg';
import sendIcon from '../../../assets/images/wallet-nav/send-ic-light.svg';
import sendIconActive from '../../../assets/images/wallet-nav/send-ic-dark.svg';
import receiveIcon from '../../../assets/images/wallet-nav/receive-ic-light.svg';
import receiveIconActive from '../../../assets/images/wallet-nav/receive-ic-dark.svg';
import transactionsIcon from '../../../assets/images/wallet-nav/transactions-ic-light.svg';
import transactionsIconActive from '../../../assets/images/wallet-nav/transactions-ic-dark.svg';

const messages = defineMessages({
  summary: {
    id: 'wallet.navigation.summary',
    defaultMessage: '!!!Summary',
    description: 'Label for the "Summary" nav button in the wallet navigation.'
  },
  send: {
    id: 'wallet.navigation.send',
    defaultMessage: '!!!Send',
    description: 'Label for the "Send" nav button in the wallet navigation.'
  },
  receive: {
    id: 'wallet.navigation.receive',
    defaultMessage: '!!!Receive',
    description: 'Label for the "Receive" nav button in the wallet navigation.'
  },
  transactions: {
    id: 'wallet.navigation.transactions',
    defaultMessage: '!!!Transactions',
    description: 'Label for the "Transactions" nav button in the wallet navigation.'
  }
});

@observer
export default class WalletNavigation extends Component {

  static propTypes = {
    isActiveNavItem: PropTypes.func.isRequired,
    onNavItemClick: PropTypes.func
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { isActiveNavItem, onNavItemClick } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>

        <div className={styles.navItem}>
          <WalletNavButton
            className="summary"
            label={intl.formatMessage(messages.summary)}
            normalIcon={summaryIcon}
            activeIcon={summaryIconActive}
            isActive={isActiveNavItem('home')}
            onClick={() => onNavItemClick('home')}
          />
        </div>

        <div className={styles.navItem}>
          <WalletNavButton
            className="send"
            label={intl.formatMessage(messages.send)}
            normalIcon={sendIcon}
            activeIcon={sendIconActive}
            isActive={isActiveNavItem('send')}
            onClick={() => onNavItemClick('send')}
          />
        </div>

        <div className={styles.navItem}>
          <WalletNavButton
            className="receive"
            label={intl.formatMessage(messages.receive)}
            normalIcon={receiveIcon}
            activeIcon={receiveIconActive}
            isActive={isActiveNavItem('receive')}
            onClick={() => onNavItemClick('receive')}
          />
        </div>

        <div className={styles.navItem}>
          <WalletNavButton
            label={intl.formatMessage(messages.transactions)}
            normalIcon={transactionsIcon}
            activeIcon={transactionsIconActive}
            isActive={isActiveNavItem('transactions')}
            onClick={() => onNavItemClick('transactions')}
          />
        </div>

      </div>
    );
  }
}
