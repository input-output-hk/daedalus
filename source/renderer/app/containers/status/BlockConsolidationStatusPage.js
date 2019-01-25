// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import { getCurrentEpoch } from '../../utils/network';

@inject('stores', 'actions') @observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {

  constructor(props: any) {
    super(props);
    this.props.actions.networkStatus.getNumberOfEpochsConsolidated.trigger();
  }

  render() {
    const { app, networkStatus } = this.props.stores;
    const { epochsConsolidated, syncProgress, systemStartTime } = networkStatus;
    const { openExternalLink } = app;

    return (
      <BlockConsolidationStatus
        currentEpoch={getCurrentEpoch(systemStartTime)}
        epochsConsolidated={epochsConsolidated}
        epochsSynced={syncProgress}
        onExternalLinkClick={openExternalLink}
      />
    );
  }

}
