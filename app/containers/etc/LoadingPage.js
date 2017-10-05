// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages } from 'react-intl';
import CenteredLayout from '../../components/layout/CenteredLayout';
import Loading from '../../components/loading/Loading';
import type { StoresMap } from '../../stores/index';
import etcLogo from '../../assets/images/etc-logo.inline.svg';

const messages = defineMessages({
  loadingAccountData: {
    id: 'loading.screen.etc.loadingAccountData',
    defaultMessage: '!!!Loading account data',
    description: 'Message "Loading account data" on the etc loading screen.'
  },
});

@inject(['stores']) @observer
export default class LoadingPage extends Component {

  props: {
    stores: StoresMap,
  };

  render() {
    const { stores } = this.props;
    const {
      isConnecting, isSyncing, syncPercentage, isLoadingWallets,
      hasBeenConnected, hasBlockSyncingStarted,
    } = stores.ada.networkStatus;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = stores.app;
    return (
      <CenteredLayout>
        <Loading
          currencyIcon={etcLogo}
          currencyIconWhite={etcLogo}
          isSyncing={isSyncing}
          isConnecting={isConnecting}
          syncPercentage={syncPercentage}
          isLoadingDataForNextScreen={isLoadingWallets}
          loadingDataForNextScreenMessage={messages.loadingAccountData}
          hasBeenConnected={hasBeenConnected}
          hasBlockSyncingStarted={hasBlockSyncingStarted}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
        />
      </CenteredLayout>
    );
  }
}
