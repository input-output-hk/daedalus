// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { shell } from 'electron';
import CenteredLayout from '../../components/layout/CenteredLayout';
import WalletBugReportDialog from '../../components/wallet/WalletBugReportDialog';
import WalletSupportRequestPage from '../../containers/wallet/WalletSupportRequestPage';
import Loading from '../../components/loading/Loading';
import etcLogo from '../../assets/images/etc-logo.inline.svg';
import mantisLogo from '../../assets/images/mantis-logo.inline.svg';
import { messages } from '../LoadingPage';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class LoadingPage extends Component<InjectedProps> {

  render() {
    const { stores } = this.props;
    const {
      isConnecting, isSyncing, syncPercentage, isLoadingWallets,
      hasBeenConnected, hasBlockSyncingStarted, localTimeDifference,
      ALLOWED_TIME_DIFFERENCE,
    } = stores.networkStatus;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme, currentLocale } = stores.profile;
    return (
      <CenteredLayout>
        <Loading
          currencyIcon={etcLogo}
          apiIcon={mantisLogo}
          isSyncing={isSyncing}
          localTimeDifference={localTimeDifference}
          allowedTimeDifference={ALLOWED_TIME_DIFFERENCE}
          isConnecting={isConnecting}
          syncPercentage={syncPercentage}
          isLoadingDataForNextScreen={isLoadingWallets}
          loadingDataForNextScreenMessage={messages.loadingWalletData}
          hasBeenConnected={hasBeenConnected}
          hasBlockSyncingStarted={hasBlockSyncingStarted}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          currentLocale={currentLocale}
          handleReportIssue={this.handleReportIssue}
          onProblemSolutionClick={this.handleProblemSolutionClick}
        />
        <WalletSupportRequestPage />
      </CenteredLayout>
    );
  }

  handleReportIssue = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletBugReportDialog
    });
  }

  handleProblemSolutionClick = (link: string) => {
    shell.openExternal(`https://${link}`);
  }
}
