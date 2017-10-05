// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages } from 'react-intl';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import type { StoresMap } from '../stores/index';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import cardanoLogoWhite from '../assets/images/cardano-logo-white.inline.svg';

const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.ada.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the ada loading screen.'
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
          currencyIcon={cardanoLogo}
          currencyIconWhite={cardanoLogoWhite}
          isSyncing={isSyncing}
          isConnecting={isConnecting}
          syncPercentage={syncPercentage}
          isLoadingDataForNextScreen={isLoadingWallets}
          loadingDataForNextScreenMessage={messages.loadingWalletData}
          hasBeenConnected={hasBeenConnected}
          hasBlockSyncingStarted={hasBlockSyncingStarted}
          hasLoadedCurrentLocale={hasLoadedCurrentLocale}
          hasLoadedCurrentTheme={hasLoadedCurrentTheme}
        />
      </CenteredLayout>
    );
  }
}
