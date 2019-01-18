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

  handleExternalLinkClick = (event: MouseEvent) => {
    event.preventDefault();
    if (event.target.href) shell.openExternal(event.target.href);
  };

  render() {
    return (
      <BlockConsolidationStatus
        onExternalLinkClick={this.handleExternalLinkClick}
      />
    );
  }

}
