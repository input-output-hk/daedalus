// @flow
import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import QRCode from 'qrcode.react';
import RaisedButton from 'material-ui/RaisedButton';
import styles from './WalletReceive.scss';

const messages = defineMessages({
  walletReceivePageTitle: {
    id: 'wallet.receive.page.title',
    defaultMessage: '!!!Title - Your shopping wallet address',
    description: 'Title for the wallet receive page'
  },
  walletReceiveInstructions: {
    id: 'wallet.receive.page.instructions',
    defaultMessage: '!!!Wallet receive instructions',
    description: 'Title of the receive page'
  },
  generateNewAddressLabel: {
    id: 'wallet.receive.page.generate.new.address.label',
    defaultMessage: '!!!Wallet receive instructions',
    description: 'Title of the receive page'
  },
  requestSpecificAmountButtonLabel: {
    id: 'wallet.receive.page.request.specific.amount.button.label',
    defaultMessage: '!!!Request a specific amount',
    description: 'Label for the request a specific amount button'
  }
});

@observer
export default class WalletReceive extends Component {

  static propTypes = {
    walletReceive: React.PropTypes.shape({
      name: PropTypes.string.isRequired,
      walletAddress: PropTypes.string.isRequired
    }),
  };

  render() {
    const { walletReceive } = this.props;
    const { intl } = this.context;
    return (
      <div className={styles.component}>

        <div className={styles.heading}>
          <FormattedHTMLMessage
            {...messages.walletReceivePageTitle}
            values={{
              walletName: walletReceive.name
            }}
          />
        </div>

        <div className={styles.qrCode}>
          <QRCode
            value={walletReceive.walletAddress}
            bgColor="transparent"
            size={240}
          />
        </div>

        <div className={styles.hash}>
          {walletReceive.walletAddress}
        </div>

        <div className={styles.instructions}>
          {intl.formatMessage(messages.walletReceiveInstructions)}
          <a>{intl.formatMessage(messages.generateNewAddressLabel)}</a>.
        </div>

        <RaisedButton
          className={styles.requestButton}
          label={intl.formatMessage(messages.requestSpecificAmountButtonLabel)}
          primary
          fullWidth
        />

      </div>
    );
  }

}

WalletReceive.contextTypes = {
  intl: intlShape.isRequired,
};
