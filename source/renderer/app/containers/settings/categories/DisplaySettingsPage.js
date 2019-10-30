// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DisplaySettings from '../../../components/settings/categories/DisplaySettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class DisplaySettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  selectTheme = (values: { theme: string }) => {
    this.props.actions.profile.updateTheme.trigger(values);
  };

  render() {
    const { profile, networkStatus } = this.props.stores;
    const { currentTheme } = profile;
    const { isIncentivizedTestnet } = networkStatus;

    return (
      <DisplaySettings
        theme={currentTheme}
        selectTheme={this.selectTheme}
        isIncentivizedTestnet={isIncentivizedTestnet}
      />
    );
  }
}
