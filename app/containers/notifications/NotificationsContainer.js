// @flow
import React, { Component } from 'react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import { observer, inject } from 'mobx-react';
import { ellipsis } from '../../lib/string-helpers';
import successIcon from '../../assets/images/success-small.svg';
import NotificationMessage from '../../components/widgets/NotificationMessage';
import type { InjectedDialogContainerProps } from '../../types/injectedPropsType';

const messages = defineMessages({
  addressCopyNotificationMessage: {
    id: 'wallet.receive.page.addressCopyNotificationMessage',
    defaultMessage: '!!!You have successfully copied wallet address',
    description: 'Message for the wallet address copy success notification.',
  },
});

@inject('stores', 'actions') @observer
export default class NotificationsContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null };

  props: InjectedDialogContainerProps;

  render() {
    const { stores } = this.props;
    const { uiNotifications, wallets } = stores;
    let notificationMessage;
    const activeNotifications = uiNotifications.activeNotificationsList.map(
      (activeNotification, key) => {
        let message;
        if (activeNotification.notification === NotificationMessage) {
          notificationMessage = wallets.active ? (
            <FormattedHTMLMessage
              {...messages.addressCopyNotificationMessage}
              values={{ walletAddress: ellipsis(wallets.active.address, 8) }}
            />
          ) : null;
          message = (
            <NotificationMessage
              key={key}
              notificationNumber={activeNotification.key}
              icon={successIcon}
              show
            >
              {notificationMessage}
            </NotificationMessage>
          );
        }
        return message;
      }
    );
    return (
      <div>
        {activeNotifications}
      </div>
    );
  }
}
