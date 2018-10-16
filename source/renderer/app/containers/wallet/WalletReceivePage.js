// @flow
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { ellipsis } from '../../utils/strings';
import WalletReceive from '../../components/wallet/WalletReceive';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import type { InjectedProps } from '../../types/injectedPropsType';
import { ADDRESS_COPY_NOTIFICATION_DURATION } from '../../config/timingConfig';
import { ADDRESS_COPY_NOTIFICATION_ELLIPSIS } from '../../config/formattingConfig';

export const messages = defineMessages({
  message: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

type Props = InjectedProps;

type State = {
  copiedAddress: string,
};

@inject('stores', 'actions') @observer
export default class WalletReceivePage extends Component<Props, State> {

  static defaultProps = { actions: null, stores: null };

  state = {
    copiedAddress: '',
  };

  componentWillUnmount() {
    this.closeNotification();
    this.resetErrors();
  }

  handleGenerateAddress = (spendingPassword: ?string) => {
    const { wallets } = this.props.stores;
    const wallet = wallets.active;

    if (wallet) {
      this.props.actions.addresses.createAddress.trigger({
        walletId: wallet.id,
        spendingPassword,
      });
    }
  };

  resetErrors = () => {
    this.props.actions.addresses.resetErrors.trigger();
  };

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
    const { sidebar, uiNotifications, wallets, addresses } = this.props.stores;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletReceivePage.');

    const walletAddress = addresses.active ? addresses.active.id : '';
    const isWalletAddressUsed = addresses.active ? addresses.active.used : false;
    const walletAddresses = addresses.all.reverse();

    const notification = {
      id: `${wallet.id}-copyNotification`,
      duration: ADDRESS_COPY_NOTIFICATION_DURATION,
      message: (
        <FormattedHTMLMessage
          {...messages.message}
          values={{
            walletAddress: ellipsis(
              copiedAddress,
              ADDRESS_COPY_NOTIFICATION_ELLIPSIS,
              ADDRESS_COPY_NOTIFICATION_ELLIPSIS
            )
          }}
        />
      ),
    };

    return (
      <VerticalFlexContainer>

        <WalletReceive
          walletAddress={walletAddress}
          isWalletAddressUsed={isWalletAddressUsed}
          walletAddresses={walletAddresses}
          onGenerateAddress={this.handleGenerateAddress}
          onCopyAddress={(address) => {
            this.setState({ copiedAddress: address });
            actions.notifications.open.trigger({
              id: notification.id,
              duration: notification.duration,
            });
          }}
          isSidebarExpanded={sidebar.isShowingSubMenus}
          walletHasPassword={wallet.hasPassword}
          isSubmitting={addresses.createAddressRequest.isExecuting}
          error={addresses.error}
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
