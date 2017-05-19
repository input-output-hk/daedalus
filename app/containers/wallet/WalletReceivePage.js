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

// TODO: replace with generated wallet addresses from the API
const walletAddresses = [
  { value: '3ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR7', isUsed: false },
  { value: 'GfdULjtnTXQGCDE4iR73ERitrYNwfxs4R6', isUsed: false },
  { value: 'YNwfxs4R6GfdULjtnTXQGCDE4iR73ERitr', isUsed: true },
  { value: '73ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR', isUsed: false },
  { value: 'DE4iR73ERitrYNwfxs4R6GfdULjtnTXQGC', isUsed: false },
  { value: 'TXQGCDE4iR73ERitrYNwfxs4R6GfdULjtn', isUsed: false },
  { value: 'ULjtnTXQGCDE4iR73ERitrYNwfxs4R6Gfd', isUsed: false },
  { value: 'RitrYNwfxs4R6GfdULjtnTXQGCDE4iR73E', isUsed: true },
  { value: 'GfdULjtnTXQGCDE4iR73ERitrYNwfxs4R6', isUsed: true },
  { value: 'QGCDE4iR73ERitrYNwfxs4R6GfdULjtnTX', isUsed: true },
  { value: 'TXQGCDE4iR73ERitrYNwfxs4R6GfdULjtn', isUsed: false },
  { value: 'ULjtnTXQGCDE4iR73ERitrYNwfxs4R6Gfd', isUsed: false },
  { value: 'RitrYNwfxs4R6GfdULjtnTXQGCDE4iR73E', isUsed: true },
  { value: 'GfdULjtnTXQGCDE4iR73ERitrYNwfxs4R6', isUsed: true },
  { value: 'QGCDE4iR73ERitrYNwfxs4R6GfdULjtnTX', isUsed: true },
  { value: '3ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR7', isUsed: false },
  { value: 'GfdULjtnTXQGCDE4iR73ERitrYNwfxs4R6', isUsed: false },
  { value: 'YNwfxs4R6GfdULjtnTXQGCDE4iR73ERitr', isUsed: false },
  { value: '73ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR', isUsed: false },
  { value: 'DE4iR73ERitrYNwfxs4R6GfdULjtnTXQGC', isUsed: false },
];

@inject('stores', 'actions') @observer
export default class WalletReceivePage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  state = {
    copiedAddress: '',
  };

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
    const { copiedAddress } = this.state;
    const actions = this.props.actions;
    const { wallets, uiNotifications, sidebar } = this.props.stores;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletReceivePage.');

    const notification = {
      id: `${wallet.id}-copyNotification`,
      duration: config.wallets.ADDRESS_COPY_NOTIFICATION_DURATION,
      message: (
        <FormattedHTMLMessage
          {...messages.message}
          values={{ walletAddress: ellipsis(copiedAddress, 8) }}
        />
      ),
    };

    return (
      <VerticalFlexContainer>

        <WalletReceive
          walletAddresses={walletAddresses}
          onCopyAddress={(address) => {
            this.setState({ copiedAddress: address });
            actions.notifications.open.trigger({
              id: notification.id,
              duration: notification.duration,
            });
          }}
          isSidebarExpanded={sidebar.isShowingSubMenus}
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
