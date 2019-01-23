// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';
import { getCurrentEpoch } from '../../utils/network';

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class BlockConsolidationStatusPage extends Component<InjectedProps> {

  constructor(props) {
    super(props);
    this.props.actions.networkStatus.getNumberOfEpochsConsolidated.trigger();
  }

  handleClose = () => {
    const { actions } = this.props;
    actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

  handleExternalLinkClick = (url: string, event?: MouseEvent) => {
    if (event) event.preventDefault();
    shell.openExternal(url);
  };

  render() {

    const { epochsConsolidated, syncProgress /* , _startTime */ } =
      this.props.stores.networkStatus;

    return (
      <BlockConsolidationStatus
        onExternalLinkClick={this.handleExternalLinkClick}
        epochsConsolidated={epochsConsolidated}
        currentEpoch={getCurrentEpoch()}
        epochsSynced={syncProgress}
      />
    );
  }

}
