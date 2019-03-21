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
    const { wallets, walletSettings } = this.props.stores;
    const { walletUtxos } = walletSettings;
    const { active: wallet } = wallets;
    if (!wallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    return <WalletUtxoSettings walletUtxos={walletUtxos} wallet={wallet} />;
  }
}
