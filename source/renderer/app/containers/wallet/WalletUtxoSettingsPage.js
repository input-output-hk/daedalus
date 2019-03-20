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

  constructor(props: any) {
    super(props);
    this.props.actions.walletSettings.getWalletUtxos.trigger();
  }

  render() {
    const { walletUtxos } = this.props.stores.walletSettings;
    return <WalletUtxoSettings walletUtxos={walletUtxos} />;
  }
}
