// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import QRCode from 'qrcode.react';
import FullWidthButton from '../widgets/FullWidthButton';
import styles from './WalletReceive.scss';

const messages = defineMessages({
  walletReceivePageTitle: {
    id: 'wallet.receive.page.title',
    defaultMessage: '!!!Title - Your shopping wallet address',
    description: 'Title for the wallet "Receive page"'
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.instructions',
    defaultMessage: '!!!Wallet receive instructions',
    description: 'Instructions for sharing wallet address to receive payments on the wallet "Receive Page"'
  },
  generateNewAddressLabel: {
    id: 'wallet.receive.page.generate.new.address.label',
    defaultMessage: '!!!Generate new address',
    description: 'Label for "Generate new address" link'
  },
  requestSpecificAmountButtonLabel: {
    id: 'wallet.receive.page.request.specific.amount.button.label',
    defaultMessage: '!!!Request a specific amount',
    description: 'Label for the "Request a specific amount" button'
  }
});

@observer
export default class WalletReceive extends Component {

  static propTypes = {
    walletName: PropTypes.string.isRequired,
    walletAddress: PropTypes.string.isRequired
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { walletName, walletAddress } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>

        <div className={styles.heading}>
          <FormattedHTMLMessage
            {...messages.walletReceivePageTitle}
            values={{ walletName }}
          />
        </div>

        <div className={styles.qrCode}>
          <QRCode
            value={walletAddress}
            bgColor="transparent"
            size={240}
          />
        </div>

        <div className={styles.hash}>
          {walletAddress}
        </div>

        <div className={styles.instructions}>
          {intl.formatMessage(messages.walletReceiveInstructions)}
          &nbsp;<a>{intl.formatMessage(messages.generateNewAddressLabel)}</a>.
        </div>

        <FullWidthButton
          label={intl.formatMessage(messages.requestSpecificAmountButtonLabel)}
          onClick={null}
        />

      </div>
    );
  }

}
