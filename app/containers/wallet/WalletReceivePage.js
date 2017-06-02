// @flow
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { ellipsis } from '../../lib/string-helpers';
import config from '../../config';
import WalletReceive from '../../components/wallet/WalletReceive';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.svg';
import type { InjectedProps } from '../../types/injectedPropsType';

const messages = defineMessages({
  message: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

@inject('stores', 'actions') @observer
export default class WalletReceivePage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  componentWillUnmount() {
    this.closeNotification();
  }

  closeNotification = () => {
    const { wallets } = this.props.stores;
    const wallet = wallets.active;
    if (wallet) {
      const notificationId = `${wallet.id}-copyNotification`;
      this.props.actions.notifications.closeActiveNotification.trigger({ id: notificationId });
    }
  };

  render() {
    const actions = this.props.actions;
    const { wallets, addresses, uiNotifications } = this.props.stores;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletReceivePage.');

    const walletAddress = addresses.active ? addresses.active.id : '';

    const notification = {
      id: `${wallet.id}-copyNotification`,
      duration: config.wallets.ADDRESS_COPY_NOTIFICATION_DURATION,
      message: (
        <FormattedHTMLMessage
          {...messages.message}
          values={{ walletAddress: ellipsis(walletAddress, 8) }}
        />
      ),
    };

    return (
      <VerticalFlexContainer>

        <WalletReceive
          walletName={wallet.name}
          walletAddress={walletAddress}
          onCopyAddress={() => {
            actions.notifications.open.trigger({
              id: notification.id,
              duration: notification.duration,
            });
          }}
        />

        <NotificationMessage
          icon={successIcon}
          show={uiNotifications.isOpen(notification.id)}
        >
          {notification.message}
        </NotificationMessage>

      </VerticalFlexContainer>
    );
  }
}
