import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import NoDiskSpaceErrorPage from './NoDiskSpaceErrorPage';
import SystemTimeErrorPage from './SystemTimeErrorPage';
import SyncingConnectingPage from './SyncingConnectingPage';
import MithrilBootstrapPage from './MithrilBootstrapPage';
import type { InjectedProps } from '../../types/injectedPropsType';

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

  get shouldShowMithrilBootstrap() {
    return (
      !this.isSetupPage &&
      (this.isMithrilBootstrapDecision || this.isMithrilBootstrapActive)
    );
  }

  get isNotEnoughDiskSpace() {
    return this.networkStatus.isNotEnoughDiskSpace;
  }

  get isSystemTimeError() {
    const { isSystemTimeCorrect, isNodeStopping, isNodeStopped } =
      this.networkStatus;
    return !isSystemTimeCorrect && !isNodeStopping && !isNodeStopped;
  }

  get isMithrilBootstrapDecision() {
    const { mithrilBootstrap } = this.props.stores;
    return mithrilBootstrap.status === 'decision';
  }

  get isMithrilBootstrapActive() {
    const { mithrilBootstrap } = this.props.stores;
    const activeStatuses = [
      'preparing',
      'downloading',
      'verifying',
      'unpacking',
      'finalizing',
      'converting',
      'completed',
      'starting-node',
      'failed',
      'cancelled',
    ];
    return activeStatuses.includes(mithrilBootstrap.status);
  }

  get isSetupPage() {
    return this.props.stores.app.isSetupPage;
  }

  get networkStatus() {
    return this.props.stores.networkStatus;
  }

  render() {
    if (this.shouldShowMithrilBootstrap) {
      return (
        <CenteredLayout>
          <MithrilBootstrapPage />
        </CenteredLayout>
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
