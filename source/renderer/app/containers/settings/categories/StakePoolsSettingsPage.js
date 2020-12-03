// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import StakePoolsSettings from '../../../components/settings/categories/StakePoolsSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import type { SmashServerType } from '../../../types/stakingTypes';

@inject('stores', 'actions')
@observer
export default class StakePoolsSettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { stores, actions } = this.props;
    const { smashServerType, smashServerUrl } = stores.staking;
    const { selectSmashServerType, selectSmashServerUrl } = actions.staking;
    // If `smashServerType` is null, waits for it to be set
    if (!smashServerType) return false;
    return (
      <StakePoolsSettings
        smashServerType={smashServerType}
        smashServerUrl={smashServerUrl}
        onSelectSmashServerType={(smashServerType: SmashServerType) =>
          selectSmashServerType.trigger({ smashServerType })
        }
        onSelectSmashServerUrl={(smashServerUrl: string) =>
          selectSmashServerUrl.trigger({ smashServerUrl })
        }
      />
    );
  }
}
