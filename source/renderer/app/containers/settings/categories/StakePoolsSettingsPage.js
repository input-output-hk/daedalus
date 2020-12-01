// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import StakePoolsSettings from '../../../components/settings/categories/StakePoolsSettings';
import { SMASH_SERVER_TYPES } from '../../../config/stakingConfig';
import type { InjectedProps } from '../../../types/injectedPropsType';
import type { SmashServerType } from '../../../types/stakingTypes';

@inject('stores', 'actions')
@observer
export default class StakePoolsSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  handleSelectSmashServerType = (smashServerType: SmashServerType) => {
    const {
      selectSmashServerType,
      selectSmashServerUrl,
    } = this.props.actions.staking;
    selectSmashServerType.trigger({ smashServerType });
    if (smashServerType !== SMASH_SERVER_TYPES.CUSTOM) {
      selectSmashServerUrl.trigger({ smashServerUrl: '' });
    }
  };

  handleSelectSmashServerUrl = (smashServerUrl: string) => {
    const { selectSmashServerUrl } = this.props.actions.staking;
    selectSmashServerUrl.trigger({ smashServerUrl });
  };

  render() {
    const { stores } = this.props;
    const { smashServerType, smashServerUrl } = stores.staking;
    // If `smashServerType` is null, waits for it to be set
    if (!smashServerType) return false;
    return (
      <StakePoolsSettings
        smashServerType={smashServerType}
        smashServerUrl={smashServerUrl}
        onSelectSmashServerType={this.handleSelectSmashServerType}
        onSelectSmashServerUrl={this.handleSelectSmashServerUrl}
      />
    );
  }
}
