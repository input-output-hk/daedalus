// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SettingsLayout from '../../components/settings/SettingsLayout';
import SettingsMenu from '../../components/settings/menu/SettingsMenu';
import Layout from '../MainLayout';
import { buildRoute } from '../../utils/routing';
import type { InjectedContainerProps } from '../../types/injectedPropsType';
import GenericNotificationContainer from '../notifications/GenericNotificationContainer';
import GenericNotification from '../../components/notifications/GenericNotification';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from '../../config/timingConfig';

const messages = defineMessages({
  downloadLogsSuccess: {
    id: 'settings.support.reportProblem.downloadLogsSuccessMessage',
    defaultMessage: '!!!Logs successfully downloaded',
    description: 'Success message for download logs.',
  },
  downloadLogsProgress: {
    id: 'settings.support.reportProblem.downloadLogsProgressMessage',
    defaultMessage: '!!!Preparing logs for download',
    description: 'Progress message for download logs.',
  },
});

const DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID =
  'settings-page-download-logs-progress';
const DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID =
  'settings-page-download-logs-success';

@inject('stores', 'actions')
@observer
export default class Settings extends Component<InjectedContainerProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  isActivePage = (route: string) => {
    const { location } = this.props.stores.router;
    if (location) {
      return location.pathname === buildRoute(route);
    }
    return false;
  };

  render() {
    const { actions, children, stores } = this.props;
    const { intl } = this.context;
    const menu = (
      <SettingsMenu
        onItemClick={route => actions.router.goToRoute.trigger({ route })}
        isActiveItem={this.isActivePage}
      />
    );
    return (
      <Layout>
        <SettingsLayout menu={menu}>{children}</SettingsLayout>
        <GenericNotificationContainer>
          <GenericNotification
            id={DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID}
            show={stores.uiNotifications.isOpen(
              DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID
            )}
            actionToListenAndOpen={actions.profile.downloadLogs}
            actionToListenAndClose={actions.profile.downloadLogsSuccess}
            icon="spinner"
            hasEllipsis
            hasCloseButton
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
            icon="success"
            hasCloseButton
          >
            {intl.formatMessage(messages.downloadLogsSuccess)}
          </GenericNotification>
        </GenericNotificationContainer>
      </Layout>
    );
  }
}
