// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletUtxoSettings from '../../components/wallet/WalletUtxoSettings';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSettingsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const allStakes = 0;
    const boundType = 'Log10';
    const histogram = {
      '10': 0,
      '100': 0,
      '1000': 0,
      '10000': 0,
      '100000': 0,
      '1000000': 0,
      '10000000': 0,
      '100000000': 0,
      '1000000000': 0,
      '10000000000': 0,
      '100000000000': 0,
      '1000000000000': 0,
      '10000000000000': 0,
      '100000000000000': 0,
      '1000000000000000': 0,
      '10000000000000000': 0,
      '45000000000000000': 0,
    };

    return (
      <WalletUtxoSettings
        allStakes={allStakes}
        boundType={boundType}
        histogram={histogram}
      />
    );
  }
}
