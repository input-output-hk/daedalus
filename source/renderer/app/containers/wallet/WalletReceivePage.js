// @flow
import React, { Component, Fragment } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { ellipsis } from '../../utils/strings';
import WalletReceive from '../../components/wallet/receive/WalletReceive';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.inline.svg';
import type { InjectedProps } from '../../types/injectedPropsType';
import WalletAddress from '../../domains/WalletAddress';
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

@inject('stores', 'actions')
@observer
export default class WalletReceivePage extends Component<Props, State> {
  static defaultProps = { actions: null, stores: null };

  state = {
    copiedAddress: '',
  };

  componentWillUnmount() {
    this.closeNotification();
  }

  get activeWallet() {
    return this.props.stores.wallets.active;
  }

  get notification() {
    const { copiedAddress } = this.state;

    // Guard against potential null values
    if (!this.activeWallet)
      throw new Error('Active wallet required for WalletReceivePage.');

    return {
      id: `${this.activeWallet.id}-copyNotification`,
      duration: ADDRESS_COPY_NOTIFICATION_DURATION,
      message: (
        <FormattedHTMLMessage
          {...messages.message}
          values={{
            walletAddress: ellipsis(
              copiedAddress,
              ADDRESS_COPY_NOTIFICATION_ELLIPSIS,
              ADDRESS_COPY_NOTIFICATION_ELLIPSIS
            ),
          }}
        />
      ),
    };
  }

  closeNotification = () => {
    const { id } = this.notification;
    if (this.activeWallet) {
      this.props.actions.notifications.closeActiveNotification.trigger({ id });
    }
  };

  handleIsAddressValid = (index: number) => index < 3 || index > 7;

  handleCopyAddress = (address: string) => {
    this.setState({ copiedAddress: address });
    this.props.actions.notifications.open.trigger({
      id: this.notification.id,
      duration: this.notification.duration,
    });
  };

  handleShareAddress = () => {
    console.log('handleShareAddress');
  };

  render() {
    const { uiNotifications, addresses, networkStatus } = this.props.stores;
    const { isIncentivizedTestnet } = networkStatus.environment;

    // Guard against potential null values
    if (!this.activeWallet)
      throw new Error('Active wallet required for WalletReceivePage.');

    const walletAddresses = addresses.all.slice().reverse();

    const invalidWalletAddress: WalletAddress = new WalletAddress({
      ...walletAddresses[4],
      isInvalid: true,
    });

    const walletAddressesWithInvalidAddresses =
      walletAddresses && walletAddresses.length
        ? [
            ...walletAddresses.slice(0, 4),
            invalidWalletAddress,
            ...walletAddresses.slice(5, walletAddresses.length - 1),
          ]
        : [];

    return (
      <Fragment>
        <VerticalFlexContainer>
          <WalletReceive
            walletAddresses={walletAddressesWithInvalidAddresses}
            isAddressValid={this.handleIsAddressValid}
            onShareAddress={this.handleShareAddress}
            onCopyAddress={this.handleCopyAddress}
            isIncentivizedTestnet={isIncentivizedTestnet}
          />
        </VerticalFlexContainer>

        <NotificationMessage
          icon={successIcon}
          show={uiNotifications.isOpen(this.notification.id)}
          onClose={this.closeNotification}
          clickToClose
          hasCloseButton
        >
          {this.notification.message}
        </NotificationMessage>
      </Fragment>
    );
  }
}
