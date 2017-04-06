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
    const {
      isConnecting,
      isSyncing,
      syncPercentage,
      isLoadingWallets,
      hasBeenConnected
    } = this.props.stores.networkStatus;
    return (
      <CenteredLayout>
        <Loading
          isSyncing={isSyncing}
          isConnecting={isConnecting}
          syncPercentage={syncPercentage}
          isLoadingWallets={isLoadingWallets}
          hasBeenConnected={hasBeenConnected}
        />
      </CenteredLayout>
    );
  }
}
