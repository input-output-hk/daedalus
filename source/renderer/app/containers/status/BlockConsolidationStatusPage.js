// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';

const EPOCH_DATA_UPDATE_INTERVAL = 60 * 1000; // 60 seconds | unit: miliseconds

@inject('stores', 'actions') @observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {

  pollersInterval: IntervalID = null;

  componentWillMount() {
    this.pollersInterval = setInterval(
      this.getEpochData,
      EPOCH_DATA_UPDATE_INTERVAL
    );
    this.getEpochData();
  }

  componeneWillUnmount() {
    clearInterval(this.pollersInterval);
  }

  getEpochData = () => {
    this.props.actions.networkStatus.getEpochsData.trigger();
  }

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
      />
    );
  }

}
