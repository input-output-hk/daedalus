// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import { EPOCH_DATA_UPDATE_INTERVAL } from '../../config/timingConfig';
import { ROUTES } from '../../routes-config';

@inject('stores', 'actions') @observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {

  pollingInterval: ?IntervalID = null;

  componentWillMount() {
    this.pollingInterval = setInterval(
      this.getEpochData,
      EPOCH_DATA_UPDATE_INTERVAL
    );
    this.getEpochData();
  }

  componeneWillUnmount() {
    if (this.pollingInterval) clearInterval(this.pollingInterval);
  }

  getEpochData = () => {
    this.props.actions.networkStatus.getEpochsData.trigger();
  };

  handleClose = () => {
    this.props.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

  render() {
    const { app, networkStatus } = this.props.stores;
    const { epochsConsolidated, syncProgress, currentEpoch } = networkStatus;
    const { openExternalLink } = app;

    return (
      <BlockConsolidationStatus
        currentEpoch={currentEpoch}
        epochsConsolidated={epochsConsolidated}
        epochsSynced={syncProgress}
        onExternalLinkClick={openExternalLink}
        onClose={this.handleClose}
      />
    );
  }

}
