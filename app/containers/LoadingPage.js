// @flow
import React, { PropTypes, Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../components/layout/CenteredLayout';
import Loading from '../components/loading/Loading';

@inject(['stores']) @observer
export default class LoadingPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      networkStatus: PropTypes.shape({
        isConnecting: PropTypes.bool.isRequired,
        hasBlockSyncingStarted: PropTypes.bool.isRequired,
        isSyncing: PropTypes.bool.isRequired,
        isLoadingWallets: PropTypes.bool.isRequired,
        hasBeenConnected: PropTypes.bool.isRequired,
        syncPercentage: PropTypes.number.isRequired,
      }).isRequired,
      app: PropTypes.shape({
        hasLoadedCurrentLocale: PropTypes.bool.isRequired,
      }).isRequired,
    }).isRequired,
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
    const { hasLoadedCurrentLocale } = stores.app;
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
        />
      </CenteredLayout>
    );
  }
}
