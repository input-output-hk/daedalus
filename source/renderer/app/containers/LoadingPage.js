// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import { shell, remote } from 'electron';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import adaLogo from '../assets/images/ada-logo.inline.svg';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import type { InjectedProps } from '../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../common/fileName';
import { getSupportUrl } from '../utils/network';

export const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.'
  },
  reportIssueButtonUrl: {
    id: 'loading.screen.reportIssue.reportIssueButtonUrl',
    defaultMessage: '!!!https://iohk.zendesk.com/hc/en-us/categories/360000877653-Daedalus-wallet-mainnet',
    description: 'Link to Open Support page'
  },
});

@inject('stores', 'actions') @observer
export default class LoadingPage extends Component<InjectedProps> {

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { stores } = this.props;
    const {
      cardanoNodeState, isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference, ignoreSystemTimeChecks,
    } = stores.networkStatus;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme, currentLocale } = stores.profile;
    return (
      <CenteredLayout>
        <Loading
          currencyIcon={adaLogo}
          apiIcon={cardanoLogo}
          cardanoNodeState={cardanoNodeState}
          isConnected={isConnected}
          isSynced={isSynced}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isCheckingSystemTime={forceCheckTimeDifferenceRequest.isExecuting}
          syncPercentage={syncPercentage}
          loadingDataForNextScreenMessage={messages.loadingWalletData}
          hasBeenConnected={hasBeenConnected}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          currentLocale={currentLocale}
          onExternalLinkClick={this.handleExternalLinkClick}
          onReportIssueClick={this.handleReportIssueClick}
          onCheckTheTimeAgain={forceCheckLocalTimeDifference}
          onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
          onDownloadLogs={this.handleDownloadLogs}
        />
      </CenteredLayout>
    );
  }

  handleExternalLinkClick = (event: MouseEvent, url: string) => {
    event.preventDefault();
    shell.openExternal(url);
  };

  handleReportIssueClick = (event: MouseEvent) => {
    const { intl } = this.context;
    const reportIssueButtonUrl = intl.formatMessage(messages.reportIssueButtonUrl);
    this.handleExternalLinkClick(event, getSupportUrl(reportIssueButtonUrl));
  };

  handleDownloadLogs = () => {
    const fileName = generateFileNameWithTimestamp();
    const destination = remote.dialog.showSaveDialog({
      defaultPath: fileName,
    });
    if (destination) {
      this.props.actions.profile.downloadLogs.trigger({ fileName, destination, fresh: true });
    }
  };
}
