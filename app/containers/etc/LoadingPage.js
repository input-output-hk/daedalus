// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import Loading from '../../components/loading/etc/Loading';
import type { StoresMap } from '../../stores/index';

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
      syncPercentage,
      isLoadingWallets,
      hasBeenConnected,
      hasBlockSyncingStarted,
    } = stores.networkStatus;
    const {
      hasLoadedCurrentLocale,
      hasLoadedCurrentTheme,
    } = stores.app;
    return (
      <CenteredLayout>
        <Loading
          isSyncing={isSyncing}
          isConnecting={isConnecting}
          syncPercentage={syncPercentage}
          isLoadingWallets={isLoadingWallets}
          hasBeenConnected={hasBeenConnected}
          hasBlockSyncingStarted={hasBlockSyncingStarted}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
        />
      </CenteredLayout>
    );
  }
}
