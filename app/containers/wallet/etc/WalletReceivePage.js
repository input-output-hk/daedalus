// @flow
import React, { Component } from 'react';
import { FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { ellipsis } from '../../../utils/strings';
import config from '../../../config';
import WalletReceive from '../../../components/wallet/etc/WalletReceive';
import VerticalFlexContainer from '../../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../../components/widgets/NotificationMessage';
import successIcon from '../../../assets/images/success-small.inline.svg';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { messages } from '../WalletReceivePage';

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
  }

  closeNotification = () => {
    const { wallets } = this.props.stores.etc;
    const wallet = wallets.active;
    if (wallet) {
      const notificationId = `${wallet.id}-copyNotification`;
      this.props.actions.notifications.closeActiveNotification.trigger({ id: notificationId });
    }
  };

  render() {
    const { copiedAddress } = this.state;
    const actions = this.props.actions;
    const { uiNotifications, etc } = this.props.stores;
    const { wallets } = etc;
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
          walletAddress={wallet.id}
          onCopyAddress={(address) => {
            this.setState({ copiedAddress: address });
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
