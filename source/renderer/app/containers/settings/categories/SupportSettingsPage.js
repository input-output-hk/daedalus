// @flow
import React, { Component, Fragment } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import SupportSettings from '../../../components/settings/categories/SupportSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../common/utils/files';
import { getSupportUrl } from '../../../utils/network';
import successIcon from '../../../assets/images/success-small.inline.svg';
import NotificationMessage from '../../../components/widgets/NotificationMessage';
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
});

@inject('stores', 'actions')
@observer
export default class SupportSettingsPage extends Component<InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  constructor(props: any, context: any) {
    super(props);
    this.context = context;
    this.registerOnDownloadLogsNotification();
  }

  componentWillUnmount() {
    const { profile } = this.props.actions;
    profile.downloadLogs.remove(this.openNotification);
    this.closeNotification();
  }

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

  openNotification = () => {
    const { notifications } = this.props.actions;
    const { id, duration } = this.notification;
    notifications.open.trigger({ id, duration });
  };

  registerOnDownloadLogsNotification = () => {
    const { profile } = this.props.actions;
    profile.downloadLogs.listen(this.openNotification);
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

  get notification() {
    const { intl } = this.context;
    return {
      id: 'settings-page-download-logs-success',
      duration: DOWNLOAD_LOGS_SUCCESS_DURATION,
      message: intl.formatMessage(messages.downloadLogsSuccess),
    };
  }

  closeNotification = () => {
    const { id } = this.notification;
    this.props.actions.notifications.closeActiveNotification.trigger({ id });
  };

  render() {
    const { stores } = this.props;
    const { id, message } = this.notification;
    return (
      <Fragment>
        <SupportSettings
          onExternalLinkClick={stores.app.openExternalLink}
          onSupportRequestClick={this.handleSupportRequestClick}
          onDownloadLogs={this.handleDownloadLogs}
        />
        <NotificationMessage
          icon={successIcon}
          show={stores.uiNotifications.isOpen(id)}
          onClose={this.closeNotification}
          clickToClose
          hasCloseButton
        >
          {message}
        </NotificationMessage>
      </Fragment>
    );
  }
}
