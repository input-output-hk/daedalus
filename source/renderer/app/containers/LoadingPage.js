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
import GenericNotificationContainer from './notifications/GenericNotificationContainer';
import GenericNotification from '../components/notifications/GenericNotification';
import { DOWNLOAD_LOGS_SUCCESS_DURATION } from '../config/timingConfig';

export const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.',
  },
  reportIssueButtonUrl: {
    id: 'loading.screen.reportIssue.reportIssueButtonUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/requests/new/',
    description: 'Link to Open Support page',
  },
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

type State = {
  disableDownloadLogs: boolean,
};

const DOWNLOAD_LOGS_PROGRESS_NOTIFICATION_ID =
  'loading-page-download-logs-progress';
const DOWNLOAD_LOGS_SUCCESS_NOTIFICATION_ID =
  'loading-page-download-logs-success';

@inject('stores', 'actions')
@observer
export default class LoadingPage extends Component<InjectedProps, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  constructor(props: InjectedProps) {
    super(props);
    const { profile } = this.props.actions;
    profile.downloadLogsSuccess.listen(() =>
      this.toggleDisableDownloadLogs(false)
    );
  }

  state = {
    disableDownloadLogs: false,
  };

  render() {
    const { stores, actions } = this.props;
    const { intl } = this.context;
    const {
      cardanoNodeState,
      isNodeResponding,
      isNodeSubscribed,
      isNodeSyncing,
      isNodeTimeCorrect,
      isConnected,
      isSynced,
      syncPercentage,
      hasBeenConnected,
      localTimeDifference,
      isSystemTimeCorrect,
      forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference,
      ignoreSystemTimeChecks,
      isNodeStopping,
      isNodeStopped,
      isNotEnoughDiskSpace,
      isTlsCertInvalid,
      diskSpaceRequired,
      diskSpaceMissing,
      diskSpaceRecommended,
    } = stores.networkStatus;
    const {
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      currentLocale,
    } = stores.profile;
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
          isTlsCertInvalid={isTlsCertInvalid}
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
          isNodeResponding={isNodeResponding}
          isNodeSubscribed={isNodeSubscribed}
          isNodeSyncing={isNodeSyncing}
          isNodeTimeCorrect={isNodeTimeCorrect}
          onReportIssueClick={this.handleReportIssueClick}
          onCheckTheTimeAgain={forceCheckLocalTimeDifference}
          onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
          onDownloadLogs={this.handleDownloadLogs}
          disableDownloadLogs={this.state.disableDownloadLogs}
        />
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
      </CenteredLayout>
    );
  }

  handleReportIssueClick = async (event: SyntheticEvent<HTMLButtonElement>) => {
    event.persist();
    const { intl } = this.context;
    const reportIssueButtonUrl = intl.formatMessage(
      messages.reportIssueButtonUrl
    );
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(reportIssueButtonUrl, locale);
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleDownloadLogs = () => {
    const { profile } = this.props.actions;
    const fileName = generateFileNameWithTimestamp();
    const destination = global.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      this.toggleDisableDownloadLogs(true);
      profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
  };

  toggleDisableDownloadLogs = (disableDownloadLogs: boolean) => {
    this.setState({ disableDownloadLogs });
  };
}
