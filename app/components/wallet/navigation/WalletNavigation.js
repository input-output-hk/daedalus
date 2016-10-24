// @flow
import React, { Component, PropTypes } from 'react';
import { Link } from 'react-router';
import { observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import styles from './WalletNavigation.scss';
import WalletNavHomeButton from './WalletHomeButton';
import WalletNavButton from './WalletNavButton';

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
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { wallet } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>

        <Link to={'/wallet/home'} className={styles.homeLink}>
          <WalletNavHomeButton
            className={styles.button}
            walletName={wallet.name}
            amount={wallet.amount}
            currency={wallet.currency}
            isActive={false}
          />
        </Link>

        <Link to={'/wallet/send'} className={styles.sendLink}>
          <WalletNavButton
            label={intl.formatMessage(messages.send)}
            normalIcon="./assets/images/send-ic.svg"
            activeIcon="./assets/images/send-white-ic.svg"
            className={styles.button}
            isActive
          />
        </Link>

        <Link to={'/wallet/receive'} className={styles.receiveLink}>
          <WalletNavButton
            label={intl.formatMessage(messages.receive)}
            normalIcon="./assets/images/receive-ic.svg"
            activeIcon="./assets/images/receive-white-ic.svg"
            className={styles.button}
            isActive={false}
          />
        </Link>

      </div>
    );
  }
}
