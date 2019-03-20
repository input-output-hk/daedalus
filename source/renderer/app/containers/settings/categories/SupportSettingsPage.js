// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../common/utils/files';
import { getSupportUrl } from '../../../utils/network';
import GenericNotification from '../../../components/notifications/GenericNotification';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from '../../../config/timingConfig';

const messages = defineMessages({
  supportRequestLinkUrl: {
    id: 'settings.support.reportProblem.linkUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description:
      '"submit a support request" link URL in the "Report a problem" section on the support settings page.',
  },
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
export default class SupportSettingsPage extends Component<InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  handleSupportRequestClick = async (
    event: SyntheticEvent<HTMLButtonElement>
  ) => {
    event.persist();
    const { intl } = this.context;
    const supportRequestLinkUrl = intl.formatMessage(
      messages.supportRequestLinkUrl
    );
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(supportRequestLinkUrl, locale);
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleDownloadLogs = () => {
    // TODO: refactor this direct access to the dialog api
    const fileName = generateFileNameWithTimestamp();
    const { profile } = this.props.actions;
    const destination = global.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
  };

  render() {
    const { stores, actions } = this.props;
    const { intl } = this.context;
    // const { id, message, hasCloseButton, icon } = this.notification;

    return (
      <Fragment>
        <SupportSettings
          onExternalLinkClick={stores.app.openExternalLink}
          onSupportRequestClick={this.handleSupportRequestClick}
          onDownloadLogs={this.handleDownloadLogs}
        />
        <GenericNotification
          id={DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID}
          show={stores.uiNotifications.isOpen(
            DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID
          )}
          icon="spinner"
          actionToListenAndOpen={actions.profile.downloadLogs}
          actionToListenAndClose={actions.profile.downloadLogsSuccess}
          openNotification={actions.notifications.open}
          closeNotification={actions.notifications.closeActiveNotification}
          hasEllipsis
        >
          {intl.formatMessage(messages.downloadLogsProgress)}
        </GenericNotification>
        <GenericNotification
          id={DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID}
          duration={DOWNLOAD_LOGS_SUCCESS_DURATION}
          show={stores.uiNotifications.isOpen(
            DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID
          )}
          icon="success"
          actionToListenAndOpen={actions.profile.downloadLogsSuccess}
          actionToListenAndClose={actions.profile.downloadLogs}
          openNotification={actions.notifications.open}
          closeNotification={actions.notifications.closeActiveNotification}
          hasCloseButton
        >
          {intl.formatMessage(messages.downloadLogsSuccess)}
        </GenericNotification>
      </Fragment>
    );
  }
}
