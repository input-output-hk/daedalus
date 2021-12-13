import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DisplaySettings from '../../../components/settings/categories/DisplaySettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
class DisplaySettingsPage extends Component<InjectedProps> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  selectTheme = (values: { theme: string }) => {
    this.props.actions.profile.updateTheme.trigger(values);
  };

  render() {
    const { currentTheme } = this.props.stores.profile;
    return (
      <DisplaySettings theme={currentTheme} selectTheme={this.selectTheme} />
    );
  }
}

export default DisplaySettingsPage;
