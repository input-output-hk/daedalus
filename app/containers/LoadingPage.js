// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import type { StoresMap } from '../stores/index';

@inject(['stores']) @observer
export default class LoadingPage extends Component {

  props: {
    stores: StoresMap,
  };

  render() {
    const { stores } = this.props;
    const {
      isConnecting,
      isSyncing,
      localTimeDifference,
      syncPercentage,
      isLoadingWallets,
      hasBeenConnected,
      hasBlockSyncingStarted,
      ALLOWED_TIME_DIFFERENCE,
    } = stores.networkStatus;
    const {
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
      currentLocale,
    } = stores.app;
    return (
      <CenteredLayout>
        <Loading
          isSyncing={isSyncing}
          localTimeDifference={localTimeDifference}
          allowedTimeDifference={ALLOWED_TIME_DIFFERENCE}
          isConnecting={isConnecting}
          syncPercentage={syncPercentage}
          isLoadingWallets={isLoadingWallets}
          hasBeenConnected={hasBeenConnected}
          hasBlockSyncingStarted={hasBlockSyncingStarted}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
          currentLocale={currentLocale}
        />
      </CenteredLayout>
    );
  }
}
