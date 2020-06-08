// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
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

  // eslint-disable-next-line
  UNSAFE_componentWillMount() {
    this.props.actions.walletSettings.startWalletUtxoPolling.trigger();
  }

  componentWillUnmount() {
    this.props.actions.walletSettings.stopWalletUtxoPolling.trigger();
  }

  render() {
    const { app, wallets, walletSettings, transactions } = this.props.stores;
    const { walletUtxos } = walletSettings;
    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletUtxoPage.');

    const distribution = get(walletUtxos, 'distribution', {});
    const chartData = getUtxoChartData(distribution);
    const walletUtxosAmount = getWalletUtxosTotalAmount(distribution);
    const { pendingTransactionsCount: pendingTxnsCount } = transactions;

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
