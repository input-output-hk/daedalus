// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletUtxoSettings from '../../components/wallet/settings-utxo/WalletUtxoSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import {
  getUtxoChartData,
  getUtxoWalletPrettyAmount,
  getWalletUtxosAmount,
} from '../../utils/utxoUtils';

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
    const { histogram } = walletUtxos || { histogram: {} };
    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    const chartData = getUtxoChartData(histogram);
    const walletUtxosAmount = getWalletUtxosAmount(histogram);
    return (
      <WalletUtxoSettings
        walletAmount={activeWallet.amount}
        walletUtxosAmount={walletUtxosAmount}
        chartData={chartData}
        getUtxoWalletPrettyAmount={getUtxoWalletPrettyAmount}
      />
    );
  }
}
