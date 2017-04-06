// @flow
import React, { Component, PropTypes } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';
import Wallet from '../../domain/Wallet';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.svg';
import { ellipsis } from '../../lib/string-helpers';

const messages = defineMessages({
  message: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

@inject('stores', 'actions') @observer
export default class WalletReceivePage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet).isRequired,
        isWalletAddressCopyNotificationVisible: PropTypes.bool.isRequired,
      }).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      wallets: PropTypes.shape({
        showWalletAddressCopyNotification: PropTypes.func.isRequired,
      }),
    }).isRequired,
  };

  render() {
    const actions = this.props.actions;
    const stores = this.props.stores;
    const wallet = stores.wallets.active;
    const walletAddress = wallet.address;

    const notificationMessage = (
      <FormattedHTMLMessage
        {...messages.message}
        values={{ walletAddress: ellipsis(walletAddress, 8) }}
      />
    );

    return (
      <VerticalFlexContainer>

        <WalletReceive
          walletName={wallet.name}
          walletAddress={wallet.address}
          onCopyAddress={actions.wallets.showWalletAddressCopyNotification}
        />

        <NotificationMessage
          icon={successIcon}
          show={stores.wallets.isWalletAddressCopyNotificationVisible}
        >
          {notificationMessage}
        </NotificationMessage>

      </VerticalFlexContainer>
    );
  }

}
