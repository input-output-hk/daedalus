// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import { ROUTES } from '../../routes-config';
import type { InjectedProps } from '../../types/injectedPropsType';
import BlockConsolidationStatus from '../../components/status/BlockConsolidationStatus';

const shell = require('electron').shell;

@inject('stores', 'actions') @observer
export default class NetworkStatusPage extends Component<InjectedProps> {

  handleClose = () => {
    const { actions } = this.props;
    actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

  handleExternalLinkClick = (url, event?: MouseEvent) => {
    if (event) event.preventDefault();
    shell.openExternal(url);
  };

  render() {
    return (
      <BlockConsolidationStatus
        onExternalLinkClick={this.handleExternalLinkClick}
        epochsConsolidated={75}
        epochsDownloaded={93}
        totalEpochs={95}
        epochsSynced={80}
      />
    );
  }

}
