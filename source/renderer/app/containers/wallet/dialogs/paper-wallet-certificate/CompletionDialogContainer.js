// @flow
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import CompletionDialog from '../../../../components/wallet/paper-wallet-certificate/CompletionDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';
import NotificationMessage from '../../../../components/widgets/NotificationMessage';
import successIcon from '../../../../assets/images/success-small.inline.svg';
import { ellipsis } from '../../../../utils/strings';
import { ADDRESS_COPY_NOTIFICATION_DURATION } from '../../../../config/timingConfig';
import { ADDRESS_COPY_NOTIFICATION_ELLIPSIS } from '../../../../config/formattingConfig';

export const messages = defineMessages({
  message: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

type Props = InjectedDialogContainerProps;

type State = {
  copiedAddress: string,
};

@inject('stores', 'actions') @observer
export default class CompletionDialogContainer extends Component<Props, State> {
  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  state = {
    copiedAddress: '',
  };

  render() {
    const { copiedAddress } = this.state;
    const { ada, uiNotifications } = this.props.stores;
    const actions = this.props.actions;

    const { wallets } = ada;
    const { walletCertificateAddress } = wallets;
    const wallet = wallets.active;

    const notification = {
      id: `${wallet.id}-copyNotification`,
      duration: ADDRESS_COPY_NOTIFICATION_DURATION,
      message: (
        <FormattedHTMLMessage
          {...messages.message}
          values={{ walletAddress: ellipsis(copiedAddress, ADDRESS_COPY_NOTIFICATION_ELLIPSIS) }}
        />
      ),
    };

    return (
      <div>
        <CompletionDialog
          walletCertificateAddress={walletCertificateAddress}
          onClose={this.props.onClose}
          onOpenExternalLink={this.props.stores.app.openExternalLink}
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
      </div>
    );
  }
}
