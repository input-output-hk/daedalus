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
    description: '"submit a support request" link URL in the "Report a problem" section on the support settings page.',
  },
  downloadLogsSuccess: {
    id: 'settings.support.reportProblem.downloadLogsSuccessMessage',
    defaultMessage: '!!!Logs successfully downloaded',
    description: 'Success message for download logs.',
  },
  downloadLogsProgress: {
    id: 'settings.support.reportProblem.downloadLogsProgressMessage',
    defaultMessage: '!!!Preparing logs for download...',
    description: 'Progress message for download logs.',
  },
});

type State = {
  currentNotification?: string,
};

const PROGRESS_NOTIFICATION_ID = 'settings-page-download-logs-progress';
const SUCCESS_NOTIFICATION_ID = 'settings-page-download-logs-success';

@inject('stores', 'actions') @observer
export default class SupportSettingsPage extends Component<InjectedProps, State> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = { actions: null, stores: null };

  state = {
    currentNotification: PROGRESS_NOTIFICATION_ID,
  }

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

  handleSupportRequestClick = async (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const supportRequestLinkUrl = intl.formatMessage(messages.supportRequestLinkUrl);
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(supportRequestLinkUrl, locale);
    this.props.stores.app.openExternalLink(supportUrl);
  };

  openNotification = async (id: string) => {
    console.log('openNotification', id);
    await this.setState({
      currentNotification: id
    });
    const { duration } = this.notification;
    const { notifications } = this.props.actions;
    console.log('duration', duration);
    notifications.open.trigger({ id, duration });
  };

  registerOnDownloadLogsNotification = () => {
    const { profile } = this.props.actions;
    // this.closeNotification(PROGRESS_NOTIFICATION_ID);
    // profile.downloadLogs.listen(this.openNotification(SUCCESS_NOTIFICATION_ID));
  };

  handleDownloadLogs = () => {
    // TODO: refactor this direct access to the dialog api
    const fileName = generateFileNameWithTimestamp();
    const { profile } = this.props.actions;
    const destination = global.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      this.openNotification(PROGRESS_NOTIFICATION_ID);
      profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
  };

  get notification() {
    return this[this.state.currentNotification];
  }

  get [PROGRESS_NOTIFICATION_ID]() {
    const { intl } = this.context;
    return {
      id: PROGRESS_NOTIFICATION_ID,
      message: intl.formatMessage(messages.downloadLogsProgress),
      // icon: successIcon,
    };
  }

  get [SUCCESS_NOTIFICATION_ID]() {
    const { intl } = this.context;
    return {
      id: SUCCESS_NOTIFICATION_ID,
      duration: DOWNLOAD_LOGS_SUCCESS_DURATION,
      message: intl.formatMessage(messages.downloadLogsSuccess),
      hasCloseButton: true,
      icon: successIcon,
    };
  }

  closeNotification = (id) => (
    this.props.actions.notifications.closeActiveNotification.trigger({ id })
  );

  render() {
    const { stores } = this.props;
    const { id, message, hasCloseButton, icon } = this.notification;
    return (
      <Fragment>
        <SupportSettings
          onExternalLinkClick={stores.app.openExternalLink}
          onSupportRequestClick={this.handleSupportRequestClick}
          onDownloadLogs={this.handleDownloadLogs}
        />
        <NotificationMessage
          icon={icon}
          show={stores.uiNotifications.isOpen(id)}
          hasCloseButton={hasCloseButton}
          onClose={this.closeNotification}
          clickToClose
        >
          {message}
        </NotificationMessage>
      </Fragment>
    );
  }

}
