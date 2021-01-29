// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import StakePoolsSettings from '../../../components/settings/categories/StakePoolsSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class StakePoolsSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleSelectSmashServerUrl = (smashServerUrl: string) => {
    this.props.actions.staking.selectSmashServerUrl.trigger({ smashServerUrl });
  };

  render() {
    const { stores, actions } = this.props;
    const {
      smashServerUrl,
      smashServerUrlError,
      smashServerLoading,
    } = stores.staking;
    const { openExternalLink } = stores.app;
    const { resetSmashServerError } = actions.staking;
    // If `smashServerUrl` is null, waits for it to be set
    if (!smashServerUrl) return false;
    return (
      <StakePoolsSettings
        smashServerUrl={smashServerUrl}
        smashServerUrlError={smashServerUrlError}
        onSelectSmashServerUrl={this.handleSelectSmashServerUrl}
        onResetSmashServerError={resetSmashServerError.trigger}
        isLoading={smashServerLoading}
        onOpenExternalLink={openExternalLink}
      />
    );
  }
}
