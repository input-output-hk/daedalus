// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import styles from './WalletAddressCopyNotification.scss';
import successIcon from '../../assets/images/success-small.svg';

const messages = defineMessages({
  message: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

@observer
export default class WalletAddressCopyNotification extends Component {

  static propTypes = {
    walletAddress: PropTypes.string.isRequired,
  };

  render() {
    const { walletAddress } = this.props;
    return (
      <div className={styles.component}>

        <img className={styles.icon} src={successIcon} role="presentation" />

        <span className={styles.message}>
          <FormattedHTMLMessage
            {...messages.message}
            values={{ walletAddress }}
          />
        </span>

      </div>
    );
  }

}
