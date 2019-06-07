// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import type { InjectedProps } from '../../types/injectedPropsType';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from '../../config/timingConfig';
import GenericNotification from '../../components/notifications/GenericNotification';

export const messages = defineMessages({
  downloadLogsSuccess: {
    id: 'loading.screen.reportIssue.downloadLogsSuccessMessage',
    defaultMessage: '!!!Logs successfully downloaded',
    description: 'Success message for download logs.',
  },
  downloadLogsProgress: {
    id: 'loading.screen.reportIssue.downloadLogsProgressMessage',
    defaultMessage: '!!!Preparing logs for download',
    description: 'Progress message for download logs.',
  },
});

const DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID =
  'loading-page-download-logs-progress';
const DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID =
  'loading-page-download-logs-success';

@inject('stores', 'actions')
@observer
export default class GenericNotificationContainer extends Component<InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { intl } = this.context;
    // Notification component z-index
    const notificationOrder = 99999;

    return (
      <div>
        <GenericNotification
          id={DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID}
          show={stores.uiNotifications.isOpen(
            DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID
          )}
          actionToListenAndOpen={actions.profile.downloadLogs}
          actionToListenAndClose={actions.profile.downloadLogsSuccess}
          openNotification={actions.notifications.open}
          closeNotification={actions.notifications.closeActiveNotification}
          icon="spinner"
          hasEllipsis
          hasCloseButton
          order={notificationOrder}
        >
          {intl.formatMessage(messages.downloadLogsProgress)}
        </GenericNotification>
        <GenericNotification
          id={DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID}
          duration={DOWNLOAD_LOGS_SUCCESS_DURATION}
          show={stores.uiNotifications.isOpen(
            DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID
          )}
          actionToListenAndOpen={actions.profile.downloadLogsSuccess}
          actionToListenAndClose={actions.profile.downloadLogs}
          openNotification={actions.notifications.open}
          closeNotification={actions.notifications.closeActiveNotification}
          icon="success"
          hasCloseButton
          order={notificationOrder}
        >
          {intl.formatMessage(messages.downloadLogsSuccess)}
        </GenericNotification>
      </div>
    );
  }
}
