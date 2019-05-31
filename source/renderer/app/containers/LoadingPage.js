// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import adaLogo from '../assets/images/ada-logo.inline.svg';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import type { InjectedProps } from '../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { getSupportUrl } from '../utils/network';
import NotificationMessage from '../components/widgets/NotificationMessage';
import successIcon from '../assets/images/success-small.inline.svg';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from '../config/timingConfig';

export const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.'
  },
  reportIssueButtonUrl: {
    id: 'loading.screen.reportIssue.reportIssueButtonUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Link to Open Support page'
  },
  downloadLogsSuccess: {
    id: 'loading.screen.reportIssue.downloadLogsSuccessMessage',
    defaultMessage: '!!!Logs successfully downloaded',
    description: 'Success message for download logs.',
  },
});

@inject('stores', 'actions') @observer
export default class LoadingPage extends Component<InjectedProps> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

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

  render() {
    const { stores } = this.props;
    const {
      cardanoNodeState, isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference, ignoreSystemTimeChecks, isNodeStopping, isNodeStopped,
      isNotEnoughDiskSpace, diskSpaceRequired, diskSpaceMissing, diskSpaceRecommended,
    } = stores.networkStatus;
    const {
      isNewAppVersionLoading, isNewAppVersionAvailable,
      availableAppVersion, environment, openExternalLink,
    } = stores.app;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme, currentLocale } = stores.profile;
    const { id, message } = this.notification;
    const { version } = environment;

    return (
      <CenteredLayout>
        <Loading
          currencyIcon={adaLogo}
          apiIcon={cardanoLogo}
          cardanoNodeState={cardanoNodeState}
          isConnected={isConnected}
          isSynced={isSynced}
          isNodeStopping={isNodeStopping}
          isNodeStopped={isNodeStopped}
          isNotEnoughDiskSpace={isNotEnoughDiskSpace}
          diskSpaceRequired={diskSpaceRequired}
          diskSpaceMissing={diskSpaceMissing}
          diskSpaceRecommended={diskSpaceRecommended}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isCheckingSystemTime={forceCheckTimeDifferenceRequest.isExecuting}
          syncPercentage={syncPercentage}
          loadingDataForNextScreenMessage={messages.loadingWalletData}
          hasBeenConnected={hasBeenConnected}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          currentLocale={currentLocale}
          onExternalLinkClick={stores.app.openExternalLink}
          onReportIssueClick={this.handleReportIssueClick}
          onCheckTheTimeAgain={forceCheckLocalTimeDifference}
          onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
          onDownloadLogs={this.handleDownloadLogs}
          isNewAppVersionLoading={isNewAppVersionLoading}
          isNewAppVersionAvailable={isNewAppVersionAvailable}
          currentAppVersion={version}
          availableAppVersion={availableAppVersion}
          onGetAvailableVersions={this.handleGetAvailableVersions}
          onManualUpdateInstructionsLinkClick={openExternalLink}
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
      </CenteredLayout>
    );
  }

  get notification() {
    const { intl } = this.context;
    return {
      id: 'loading-page-download-logs-success',
      duration: DOWNLOAD_LOGS_SUCCESS_DURATION,
      message: intl.formatMessage(messages.downloadLogsSuccess),
    };
  }

  closeNotification = () => {
    const { id } = this.notification;
    this.props.actions.notifications.closeActiveNotification.trigger({ id });
  };

  handleReportIssueClick = async (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const reportIssueButtonUrl = intl.formatMessage(messages.reportIssueButtonUrl);
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(reportIssueButtonUrl, locale);
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
    const { profile } = this.props.actions;
    const fileName = generateFileNameWithTimestamp();
    const destination = global.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
  };

  handleGetAvailableVersions = () => {
    const { app } = this.props.actions;
    app.getLatestAvailableAppVersion.trigger();
  };
}
