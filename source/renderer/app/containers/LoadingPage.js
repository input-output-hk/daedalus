// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages } from 'react-intl';
import { shell } from 'electron';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import BugReportDialog from '../components/profile/bug-report/BugReportDialog';
import WalletSupportRequestPage from '../containers/wallet/WalletSupportRequestPage';
import adaLogo from '../assets/images/ada-logo.inline.svg';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import type { InjectedProps } from '../types/injectedPropsType';

export const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.'
  },
});

@inject('stores', 'actions') @observer
export default class LoadingPage extends Component<InjectedProps> {

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
          handleReportIssue={this.handleReportIssue}
          onProblemSolutionClick={this.handleProblemSolutionClick}
          onCheckTheTimeAgain={forceCheckLocalTimeDifference}
          onContinueWithoutClockSyncCheck={ignoreSystemTimeChecks}
        />
        <WalletSupportRequestPage />
      </CenteredLayout>
    );
  }

  handleReportIssue = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: BugReportDialog
    });
  };

  handleProblemSolutionClick = (link: string) => {
    shell.openExternal(`https://${link}`);
  };
}
