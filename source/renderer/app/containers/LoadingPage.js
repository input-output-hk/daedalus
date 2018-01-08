// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { defineMessages } from 'react-intl';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';
import type { StoresMap } from '../stores/index';
import cardanoLogo from '../assets/images/cardano-logo.inline.svg';
import cardanoLogoWhite from '../assets/images/cardano-logo-white.inline.svg';

export const messages = defineMessages({
  loadingWalletData: {
    id: 'loading.screen.loadingWalletData',
    defaultMessage: '!!!Loading wallet data',
    description: 'Message "Loading wallet data" on the loading screen.'
  },
});

type Props = { stores: StoresMap };

@inject(['stores']) @observer
export default class LoadingPage extends Component<Props> {

  render() {
    const { stores } = this.props;
    const {
      isConnecting, isSyncing, syncPercentage, isLoadingWallets,
      hasBeenConnected, hasBlockSyncingStarted,
    } = stores.networkStatus;
    const { hasLoadedCurrentLocale, hasLoadedCurrentTheme } = stores.profile;
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
