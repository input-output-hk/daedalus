// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, FormattedHTMLMessage } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import Notification from '../../components/notifications/Notification';
import successIcon from '../../assets/images/success-small.inline.svg';
import spinnerIcon from '../../assets/images/spinner-dark.inline.svg';
import type {
  NotificationConfig,
  NotificationId,
} from '../../types/notificationTypes';
import type { NotificationMessageProps } from '../../components/notifications/Notification.js';

const ICONS = {
  successIcon,
  spinnerIcon,
};

const messages = defineMessages({
  downloadLogsProgress: {
    id: 'notification.downloadLogsProgress',
    defaultMessage: '!!!Preparing logs for download',
    description:
      'Notification for download logs in progress in the Loading and Settings pages.',
  },
  downloadLogsSuccess: {
    id: 'notification.downloadLogsSuccess',
    defaultMessage: '!!!Logs successfully downloaded',
    description:
      'Notification for download logs in the Loading and Settings pages.',
  },
  copyAddress: {
    id: 'notification.copyAddress',
    defaultMessage:
      '!!!Address: <strong>{walletAddress}</strong> copied to clipboard',
    description:
      'Notification for the wallet address copy success in the Wallet Receive page.',
  },
  copyStateDirectoryPath: {
    id: 'notification.copyStateDirectoryPath',
    defaultMessage: '!!!Daedalus state directory copied to clipboard',
    description:
      'Notification for the state directory copy success in the Diagnostics page.',
  },
});

@inject('stores', 'actions')
@observer
export default class NotificationsContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  constructor(props: InjectedProps) {
    super(props);
    this.registerNotifications();
  }

  notificationsConfig: Array<NotificationConfig> = [
    {
      id: 'downloadLogsProgress',
      actionToListenAndOpen: this.props.actions.profile.downloadLogs,
      actionToListenAndClose: this.props.actions.profile.downloadLogsSuccess,
    },
    {
      id: 'downloadLogsSuccess',
      actionToListenAndOpen: this.props.actions.profile.downloadLogsSuccess,
      actionToListenAndClose: this.props.actions.profile.downloadLogs,
    },
    {
      id: 'copyAddress',
      actionToListenAndOpen: this.props.actions.wallets.copyAddress,
    },
    {
      id: 'copyStateDirectoryPath',
      actionToListenAndOpen: this.props.actions.networkStatus
        .copyStateDirectoryPath,
    },
  ];

  notificationsMessage: {
    [key: NotificationId]: NotificationMessageProps,
  } = {
    downloadLogsProgress: {
      icon: 'spinner',
      hasEllipsis: true,
      clickToClose: false,
      hasCloseButton: false,
    },
  };

  registerNotifications = () => {
    const { registerNotification } = this.props.actions.notifications;
    this.notificationsConfig.forEach((notificationConfig: NotificationConfig) =>
      registerNotification.trigger(notificationConfig)
    );
  };

  getIcon = (icon?: string = 'success') => (icon ? ICONS[`${icon}Icon`] : icon);

  getLabel = (id: string, labelValues?: ?Object) => {
    const values = typeof labelValues === 'object' ? labelValues : {};
    return <FormattedHTMLMessage {...messages[id]} values={values} />;
  };

  render() {
    const { stores, actions } = this.props;
    const { closeNotification } = actions.notifications;
    const { activeNotifications } = stores.uiNotifications;
    Object.keys(activeNotifications);
    return (
      <div>
        {this.notificationsConfig.map(({ id }: NotificationConfig) => {
          const isVisible = id in activeNotifications;
          const message = this.notificationsMessage[id] || {};
          const { labelValues, index } = isVisible
            ? activeNotifications[id]
            : {};
          const { icon } = message || {};
          const hasSpinner = icon === 'spinner';
          return (
            <Notification
              key={id}
              {...message}
              onClose={() => closeNotification.trigger({ id })}
              icon={this.getIcon(icon)}
              isVisible={isVisible}
              hasSpinner={hasSpinner}
              index={index || 0}
            >
              {isVisible ? this.getLabel(id, labelValues) : null}
            </Notification>
          );
        })}
      </div>
    );
  }
}
