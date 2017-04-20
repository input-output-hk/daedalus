// @flow
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import WalletReceive from '../../components/wallet/WalletReceive';
import VerticalFlexContainer from '../../components/layout/VerticalFlexContainer';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import successIcon from '../../assets/images/success-small.svg';
import { ellipsis } from '../../lib/string-helpers';
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

  render() {
    const actions = this.props.actions;
    const { wallets } = this.props.stores;
    const wallet = wallets.active;

    // Guard against potential null values
    if (!wallet) throw new Error('Active wallet required for WalletReceivePage.');

    const notificationMessage = (
      <FormattedHTMLMessage
        {...messages.message}
        values={{ walletAddress: ellipsis(wallet.address, 8) }}
      />
    );

    return (
      <VerticalFlexContainer>

        <WalletReceive
          walletName={wallet.name}
          walletAddress={wallet.address}
          onCopyAddress={actions.wallets.showWalletAddressCopyNotification.trigger}
        />

        <NotificationMessage
          icon={successIcon}
          show={wallets.isWalletAddressCopyNotificationVisible}
        >
          {notificationMessage}
        </NotificationMessage>

      </VerticalFlexContainer>
    );
  }

}
