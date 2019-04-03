// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletUtxoSettings from '../../components/wallet/settings-utxo/WalletUtxoSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { Histogram } from '../../api/wallets/types';
import {
  getUtxoChartData,
  getUtxoWalletPrettyAmount,
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

  getWalletUtxosAmount = (histogram: Histogram): number => {
    const histogramArr = Object.values(histogram);
    const walletUtxosAmount = histogramArr.length
      ? histogramArr.reduce(
          (amount, value) => parseInt(amount, 10) + parseInt(value, 10)
        )
      : 0;
    return parseInt(walletUtxosAmount, 10);
  };

  render() {
    const { wallets, walletSettings } = this.props.stores;
    const { walletUtxos } = walletSettings;
    const { histogram } = walletUtxos || { histogram: {} };
    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    const chartData = getUtxoChartData(histogram);
    const walletUtxosAmount = this.getWalletUtxosAmount(histogram);
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
