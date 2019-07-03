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
export default class Loading extends Component<InjectedProps> {
  static defaultProps = { stores: null, actions: null };

  get page() {
    if (this.isNotEnoughDiskSpace) return <NoDiskSpaceErrorPage />;
    if (this.isSystemTimeError) return <SystemTimeErrorPage />;
    if (this.isManualUpdate) return <ManualUpdatePage />;
    return <SyncingConnectingPage />;
  }

  get isNotEnoughDiskSpace() {
    return this.networkStatus.isNotEnoughDiskSpace;
  }

  get isSystemTimeError() {
    const {
      isSystemTimeCorrect,
      isNodeStopping,
      isNodeStopped,
    } = this.networkStatus;
    return !isSystemTimeCorrect && !isNodeStopping && !isNodeStopped;
  }

  get isManualUpdate() {
    const { isNodeStopping, isNodeStopped } = this.networkStatus;
    const { isNewAppVersionAvailable } = this.props.stores.nodeUpdate;
    return isNewAppVersionAvailable && !isNodeStopping && !isNodeStopped;
  }

  get networkStatus() {
    return this.props.stores.networkStatus;
  }

  render() {
    return <CenteredLayout>{this.page}</CenteredLayout>;
  }
}
