// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletUtxo from '../../components/wallet/utxo/WalletUtxo';
import type { InjectedProps } from '../../types/injectedPropsType';
import {
  getUtxoChartData,
  getWalletUtxosTotalAmount,
} from '../../utils/utxoUtils';

type Props = InjectedProps;

@inject('stores', 'actions')
@observer
export default class WalletSettingsPage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  componentWillMount() {
    this.props.actions.walletSettings.startWalletUtxoPolling.trigger();
  }

  componeneWillUnmount() {
    this.props.actions.walletSettings.stopWalletUtxoPolling.trigger();
  }

  render() {
    const { app, wallets, walletSettings, transactions } = this.props.stores;
    const { walletUtxos } = walletSettings;
    const { histogram } = walletUtxos || { histogram: {} };
    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletSummaryPage.');
    const chartData = getUtxoChartData(histogram);
    const walletUtxosAmount = getWalletUtxosTotalAmount(histogram);
    const { pendingTxnsCount } = transactions;

    return (
      <WalletUtxo
        walletAmount={activeWallet.amount}
        walletUtxosAmount={walletUtxosAmount}
        chartData={chartData}
        onExternalLinkClick={app.openExternalLink}
        pendingTxnsCount={pendingTxnsCount}
      />
    );
  }
}
