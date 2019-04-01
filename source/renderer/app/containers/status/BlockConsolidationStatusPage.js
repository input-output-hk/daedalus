// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';

@inject('stores', 'actions')
@observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {
  componentWillMount() {
    this.props.actions.blockConsolidation.startBlockConsolidationDataPolling.trigger();
  }

  componeneWillUnmount() {
    this.props.actions.blockConsolidation.stopBlockConsolidationDataPolling.trigger();
  }

  handleClose = () => {
    this.props.actions.app.toggleBlockConsolidationStatusScreen.trigger();
  };

  render() {
    const { app, blockConsolidation, networkStatus } = this.props.stores;
    const { openExternalLink } = app;
    const { epochsConsolidated, currentEpoch } = blockConsolidation;
    const { syncProgress } = networkStatus;
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
