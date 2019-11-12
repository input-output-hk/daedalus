// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import CenteredLayout from '../../components/layout/CenteredLayout';
import NoDiskSpaceErrorPage from './NoDiskSpaceErrorPage';
import SystemTimeErrorPage from './SystemTimeErrorPage';
import ManualUpdatePage from './ManualUpdatePage';
import SyncingConnectingPage from './SyncingConnectingPage';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class LoadingPage extends Component<InjectedProps> {
  static defaultProps = { stores: null, actions: null };

  get activeOverlay() {
    const { showManualUpdate } = this.props.stores.nodeUpdate;
    if (this.isNotEnoughDiskSpace) return <NoDiskSpaceErrorPage />;
    if (showManualUpdate) return <ManualUpdatePage />;
    if (this.isSystemTimeError) return <SystemTimeErrorPage />;
    return null;
  }

  get isNotEnoughDiskSpace() {
    return this.networkStatus.isNotEnoughDiskSpace;
  }

  get isSystemTimeError() {
    return false;
    // const {
    //   isSystemTimeCorrect,
    //   isNodeStopping,
    //   isNodeStopped,
    // } = this.networkStatus;
    // return !isSystemTimeCorrect && !isNodeStopping && !isNodeStopped;
  }

  get networkStatus() {
    return this.props.stores.networkStatus;
  }

  render() {
    return (
      <CenteredLayout>
        {this.activeOverlay}
        <SyncingConnectingPage />
      </CenteredLayout>
    );
  }
}
