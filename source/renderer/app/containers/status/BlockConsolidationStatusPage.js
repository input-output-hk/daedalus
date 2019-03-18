// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import { EPOCH_DATA_UPDATE_INTERVAL } from '../../config/timingConfig';

const { isDevelopment } = global.environment;

@inject('stores', 'actions')
@observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {
  pollingInterval: ?IntervalID = null;
  currentEpochFallbackRequested: boolean = false;

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
    this.props.actions.app.toggleBlockConsolidationStatusScreen.trigger();
  };

  /**
   *
   * This method checks if the `currentEpoch` was returned by the API, which might fail sometimes
   * In this case, it calls `getCurrentEpochFallback`, which retrieves it from CardanoExplorer
   *
   * The reason why this is not on `componentWillReceiveProps`,
   * is that `componentWillReceiveProps` is not fired when reloading the app
   *
   */
  handleCurrentEpoch = () => {
    if (this.currentEpochFallbackRequested || isDevelopment) return false;
    const { syncProgress, currentEpoch } = this.props.stores.networkStatus;
    if (syncProgress && !currentEpoch) {
      this.currentEpochFallbackRequested = true;
      this.props.actions.networkStatus.getCurrentEpochFallback.trigger();
    }
  };

  render() {
    const { app, networkStatus } = this.props.stores;
    const { epochsConsolidated, syncProgress, currentEpoch } = networkStatus;
    this.handleCurrentEpoch();
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
