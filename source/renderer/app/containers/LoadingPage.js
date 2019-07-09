// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import adaLogo from '../assets/images/ada-logo.inline.svg';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import type { InjectedProps } from '../types/injectedPropsType';
import { getSupportUrl } from '../utils/network';

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
  readSyncIssueHelpButtonUrl: {
    id: 'loading.screen.readIssueHelp.readSyncIssueHelpButtonUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360011536933',
    description: 'Link to sync issue help page',
  },
  readConnectivityIssueHelpButtonUrl: {
    id: 'loading.screen.readIssueHelp.readConnectivityIssueHelpButtonUrl',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360010522913',
    description: 'Link to connectivity issue help page',
  },
});

@inject('stores', 'actions')
@observer
export default class LoadingPage extends Component<InjectedProps> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { stores } = this.props;
    const { environment, openExternalLink } = stores.app;
    const { version } = environment;
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
      isNewAppVersionAvailable,
      isNewAppVersionLoading,
      isNewAppVersionLoaded,
      availableAppVersion,
    } = stores.nodeUpdate;
    const {
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      currentLocale,
    } = stores.profile;

    return (
      <CenteredLayout>
        <Loading
          onStatusIconClick={this.openDaedalusDiagnosticsDialog}
          currencyIcon={adaLogo}
          apiIcon={cardanoLogo}
          cardanoNodeState={cardanoNodeState}
          currentAppVersion={version}
          availableAppVersion={availableAppVersion}
          isConnected={isConnected}
          isSynced={isSynced}
          isNodeResponding={isNodeResponding}
          isNodeSubscribed={isNodeSubscribed}
          isNodeSyncing={isNodeSyncing}
          isNodeTimeCorrect={isNodeTimeCorrect}
          isNodeStopping={isNodeStopping}
          isNodeStopped={isNodeStopped}
          isNotEnoughDiskSpace={isNotEnoughDiskSpace}
          isTlsCertInvalid={isTlsCertInvalid}
          isNewAppVersionAvailable={isNewAppVersionAvailable}
          isNewAppVersionLoading={isNewAppVersionLoading}
          isNewAppVersionLoaded={isNewAppVersionLoaded}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isCheckingSystemTime={forceCheckTimeDifferenceRequest.isExecuting}
          diskSpaceRequired={diskSpaceRequired}
          diskSpaceMissing={diskSpaceMissing}
          diskSpaceRecommended={diskSpaceRecommended}
          localTimeDifference={localTimeDifference}
          syncPercentage={syncPercentage}
          loadingDataForNextScreenMessage={messages.loadingWalletData}
          hasBeenConnected={hasBeenConnected}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          currentLocale={currentLocale}
          onExternalLinkClick={openExternalLink}
          onReportIssueClick={this.handleReportIssueClick}
          onReadSyncIssueHelpClick={this.handleReadSyncIssueHelpClick}
          onReadConnectivityIssueHelpClick={
            this.handleReadConnectivityIssueHelpClick
          }
          onCheckTheTimeAgain={forceCheckLocalTimeDifference}
          onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
          onGetAvailableVersions={this.handleGetAvailableVersions}
          onDownloadLogs={this.handleDownloadLogs}
          disableDownloadLogs={stores.app.isDownloadNotificationVisible}
        />
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

  handleReadSyncIssueHelpClick = async (
    event: SyntheticEvent<HTMLButtonElement>
  ) => {
    event.persist();
    const { intl } = this.context;
    const readSyncIssueHelpButtonUrl = intl.formatMessage(
      messages.readSyncIssueHelpButtonUrl
    );
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(readSyncIssueHelpButtonUrl, locale);
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleReadConnectivityIssueHelpClick = async (
    event: SyntheticEvent<HTMLButtonElement>
  ) => {
    event.persist();
    const { intl } = this.context;
    const readConnectivityIssueHelpButtonUrl = intl.formatMessage(
      messages.readConnectivityIssueHelpButtonUrl
    );
    const locale = this.props.stores.profile.currentLocale;
    const supportUrl = await getSupportUrl(
      readConnectivityIssueHelpButtonUrl,
      locale
    );
    this.props.stores.app.openExternalLink(supportUrl);
  };

  handleDownloadLogs = () => {
    const { app } = this.props.actions;
    app.downloadLogs.trigger();
    app.setNotificationVisibility.trigger(true);
  };

  handleGetAvailableVersions = () => {
    const { nodeUpdate } = this.props.actions;
    nodeUpdate.getLatestAvailableAppVersion.trigger();
  };

  openDaedalusDiagnosticsDialog = () => {
    const {
      actions: { app },
    } = this.props;

    app.openDaedalusDiagnosticsDialog.trigger();
  };
}
