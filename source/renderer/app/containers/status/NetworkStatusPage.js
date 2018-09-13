// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { ROUTES } from '../../routes-config';
import CenteredLayout from '../../components/layout/CenteredLayout';
import NetworkStatus from '../../components/status/NetworkStatus';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class NetworkStatusPage extends Component<InjectedProps> {

  handleClose = () => {
    const { actions } = this.props;
    actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

  render() {
    const { stores } = this.props;
    const {
      // Node state
      isNodeResponding, isNodeSubscribed, isNodeSyncing, isNodeInSync, isNodeTimeCorrect,
      // Application state
      isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, isForceCheckingNodeTime,
      forceCheckLocalTimeDifference, isSystemTimeChanged,
      localBlockHeight, networkBlockHeight,
    } = stores.networkStatus;
    return (
      <CenteredLayout>
        <NetworkStatus
          isNodeResponding={isNodeResponding}
          isNodeSubscribed={isNodeSubscribed}
          isNodeSyncing={isNodeSyncing}
          isNodeInSync={isNodeInSync}
          isNodeTimeCorrect={isNodeTimeCorrect}
          isConnected={isConnected}
          isSynced={isSynced}
          syncPercentage={syncPercentage}
          hasBeenConnected={hasBeenConnected}
          localTimeDifference={localTimeDifference}
          isSystemTimeCorrect={isSystemTimeCorrect}
          isForceCheckingNodeTime={isForceCheckingNodeTime}
          isSystemTimeChanged={isSystemTimeChanged}
          localBlockHeight={localBlockHeight}
          networkBlockHeight={networkBlockHeight}
          onForceCheckLocalTimeDifference={forceCheckLocalTimeDifference}
          onClose={this.handleClose}
        />
      </CenteredLayout>
    );
  }

}
