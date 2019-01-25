// @flow
import React, { Component } from 'react';
import { observable, action /* , when */ } from 'mobx';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import { getCurrentEpoch } from '../../utils/network';

import { getNumberOfEpochsConsolidatedChannel } from '../../ipc/getNumberOfEpochsConsolidatedChannel';
import type { GetNumberOfEpochsConsolidatedChannelResponse } from '../../../../common/ipc/api';

const EPOCH_DATA_UPDATE_INTERVAL = 20000;

@inject('stores', 'actions') @observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {

  constructor(props: any) {
    super(props);

    // const { systemStartTime } = props.stores.networkStatus;
    // when(
    //   () => systemStartTime > 0,
    //   () => this.updateEpochsData
    // );

    this.updateEpochsData();
  }

  pollersInterval: IntervalID = setInterval(
    this.updateEpochsData,
    EPOCH_DATA_UPDATE_INTERVAL
  );

  @observable epochsConsolidated: number = 0;
  @observable currentEpoch: number = 0;

  updateEpochsData = async () => {
    const { systemStartTime } = this.props.stores.networkStatus;
    this._onReceiveEpochData(
      await getNumberOfEpochsConsolidatedChannel.request(),
      getCurrentEpoch(systemStartTime),
    );
  };

  @action _onReceiveEpochData = (
    epochsConsolidated: GetNumberOfEpochsConsolidatedChannelResponse,
    currentEpoch: number,
  ) => {
    this.epochsConsolidated = epochsConsolidated;
    this.currentEpoch = currentEpoch;
  };

  render() {
    const { app, networkStatus } = this.props.stores;
    const { syncProgress } = networkStatus;
    const { openExternalLink } = app;

    return (
      <BlockConsolidationStatus
        currentEpoch={this.currentEpoch}
        epochsConsolidated={this.epochsConsolidated}
        epochsSynced={syncProgress}
        onExternalLinkClick={openExternalLink}
      />
    );
  }

}
