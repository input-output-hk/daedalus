// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';
import Wallet from '../../domain/Wallet';
import WalletAddressCopyNotification from '../../components/wallet/WalletAddressCopyNotification';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';

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

    let notificationMessage = null;
    if (stores.wallets.isWalletAddressCopyNotificationVisible) {
      notificationMessage = (
        <WalletAddressCopyNotification walletAddress={wallet.address} />
      );
    }

    return (
      <VerticalFlexContainer>
        <WalletReceive
          walletName={wallet.name}
          walletAddress={wallet.address}
          onCopyAddress={actions.wallets.showWalletAddressCopyNotification}
        />
        {notificationMessage}
      </VerticalFlexContainer>
    );
  }

}
