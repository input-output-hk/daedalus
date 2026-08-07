import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import NoDiskSpaceErrorPage from './NoDiskSpaceErrorPage';
import SystemTimeErrorPage from './SystemTimeErrorPage';
import SyncingConnectingPage from './SyncingConnectingPage';
import MithrilSyncContainer from './MithrilSyncContainer';
import ChainStorageContainer from './ChainStorageContainer';
import type { InjectedProps } from '../../types/injectedPropsType';
import styles from '../../components/loading/mithril/MithrilBootstrap.scss';

@inject('stores', 'actions')
@observer
class LoadingPage extends Component<InjectedProps> {
  static defaultProps = {
    stores: null,
    actions: null,
  };

  get activeOverlay() {
    if (this.isNotEnoughDiskSpace) return <NoDiskSpaceErrorPage />;
    if (this.isSystemTimeError) return <SystemTimeErrorPage />;
    return null;
  }

  get isNotEnoughDiskSpace() {
    return this.networkStatus.isNotEnoughDiskSpace;
  }

  get isSystemTimeError() {
    const { isSystemTimeCorrect } = this.networkStatus;
    return !isSystemTimeCorrect;
  }

  get networkStatus() {
    return this.props.stores.networkStatus;
  }

  render() {
    const { backend } = this.props.stores;
    const { loadingPhase } = backend;

    if (loadingPhase === 'chain-storage-setup') {
      return (
        <div
          style={{
            alignItems: 'center',
            backgroundColor:
              'var(--theme-mithril-overlay-backdrop-start, rgba(24, 37, 55, 1))',
            bottom: 0,
            display: 'flex',
            justifyContent: 'center',
            left: 0,
            position: 'fixed',
            right: 0,
            top: 0,
          }}
        >
          <div className={styles.card}>
            <ChainStorageContainer />
          </div>
        </div>
      );
    }

    if (
      loadingPhase === 'bootstrap-decision' ||
      loadingPhase === 'mithril-syncing'
    ) {
      return (
        <div
          style={{
            alignItems: 'center',
            backgroundColor:
              'var(--theme-mithril-overlay-backdrop-start, rgba(24, 37, 55, 1))',
            bottom: 0,
            display: 'flex',
            justifyContent: 'center',
            left: 0,
            position: 'fixed',
            right: 0,
            top: 0,
          }}
        >
          <MithrilSyncContainer />
        </div>
      );
    }

    return (
      <CenteredLayout>
        <SyncingConnectingPage />
        {this.activeOverlay}
      </CenteredLayout>
    );
  }
}

export default LoadingPage;
