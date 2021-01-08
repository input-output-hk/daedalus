// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import WalletsSettings from '../../../components/settings/categories/WalletsSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions')
@observer
export default class DisplaySettingsPage extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const {
      currencySelected,
      currencyRate,
      currencyList,
    } = this.props.stores.wallets;
    return (
      <WalletsSettings
        currencySelected={currencySelected}
        currencyRate={currencyRate}
        currencyList={currencyList}
      />
    );
  }
}
