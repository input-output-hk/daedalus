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
    const { openExternalLink } = stores.app;
    const {
      // Node state
      cardanoNodeState, isNodeResponding, isNodeSubscribed,
      isNodeSyncing, isNodeInSync, isNodeTimeCorrect,
      // Application state
      isConnected, isSynced, syncPercentage, hasBeenConnected,
      localTimeDifference, isSystemTimeCorrect, forceCheckTimeDifferenceRequest,
      forceCheckLocalTimeDifference, getNetworkStatusRequest,
      localBlockHeight, networkBlockHeight, latestLocalBlockTimestamp, latestNetworkBlockTimestamp,
      restartNode, isSystemTimeIgnored,
    } = stores.networkStatus;
    return (
      <CenteredLayout>
        Hello World
      </CenteredLayout>
    );
  }

}
