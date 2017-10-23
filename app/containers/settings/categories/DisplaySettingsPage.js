// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DisplaySettings from '../../../components/settings/categories/DisplaySettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class DisplaySettingsPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  selectTheme = (values: { theme: string }) => {
    this.props.actions.profile.updateTheme.trigger(values);
  }

  render() {
    const { currentTheme } = this.props.stores.profile;

    return (
      <DisplaySettings
        theme={currentTheme}
        selectTheme={this.selectTheme}
      />
    );
  }

}
