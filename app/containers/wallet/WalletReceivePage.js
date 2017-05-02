// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import config from '../../config';
import WalletReceive from '../../components/wallet/WalletReceive';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import NotificationsContainer from '../notifications/NotificationsContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletReceivePage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
    const actions = this.props.actions;
    const { wallets, uiNotifications } = this.props.stores;
    const wallet = wallets.active;
    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletReceivePage.');

    return (
      <VerticalFlexContainer>

        <WalletReceive
          walletName={wallet.name}
          walletAddress={wallet.address}
          onCopyAddress={() => {
            uiNotifications.countdownSinceNotificationOpened(
              config.wallets.ADDRESS_COPY_NOTIFICATION_DURATION
            );
            actions.notifications.open.trigger({ notification: NotificationMessage });
          }}
        />
        {/* TODO: move NotificationContainer to Layout continer */}
        {(uiNotifications.activeNotificationsList.length > 0) && <NotificationsContainer />}
      </VerticalFlexContainer>
    );
  }

}
