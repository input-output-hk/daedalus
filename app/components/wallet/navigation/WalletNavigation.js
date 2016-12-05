// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletNavigation.scss';
import WalletNavHomeButton from './WalletHomeButton';
import WalletNavButton from './WalletNavButton';
import sendIcon from '../../../assets/images/send-ic.svg';
import sendIconActive from '../../../assets/images/send-white-ic.svg';
import receiveIcon from '../../../assets/images/receive-ic.svg';
import receiveIconActive from '../../../assets/images/receive-white-ic.svg';

const messages = defineMessages({
  send: {
    id: 'wallet.navigation.send',
    defaultMessage: '!!!send',
    description: 'Label for the "send" nav button in the wallet navigation.'
  },
  receive: {
    id: 'wallet.navigation.receive',
    defaultMessage: '!!!receive',
    description: 'Label for the "receive" nav button in the wallet navigation.'
  }
});

@observer
export default class WalletNavigation extends Component {

  static propTypes = {
    wallet: React.PropTypes.shape({
      name: PropTypes.string.isRequired,
      amount: PropTypes.number.isRequired,
      currency: PropTypes.string.isRequired,
    }),
    isActiveNavItem: PropTypes.func.isRequired,
    onNavItemClick: PropTypes.func
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { wallet, isActiveNavItem, onNavItemClick } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>

        <div className={styles.homeLink}>
          <WalletNavHomeButton
            walletName={wallet.name}
            amount={wallet.amount}
            currency={wallet.currency}
            isActive={isActiveNavItem('home')}
            onClick={() => onNavItemClick('home')}
          />
        </div>

        <div className={styles.sendLink}>
          <WalletNavButton
            label={intl.formatMessage(messages.send)}
            normalIcon={sendIcon}
            activeIcon={sendIconActive}
            isActive={isActiveNavItem('send')}
            onClick={() => onNavItemClick('send')}
          />
        </div>

        <div className={styles.receiveLink}>
          <WalletNavButton
            label={intl.formatMessage(messages.receive)}
            normalIcon={receiveIcon}
            activeIcon={receiveIconActive}
            isActive={isActiveNavItem('receive')}
            onClick={() => onNavItemClick('receive')}
          />
        </div>

      </div>
    );
  }
}
