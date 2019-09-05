// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { get } from 'lodash';
import BigNumber from 'bignumber.js';
import WalletUtxo from '../../components/wallet/utxo/WalletUtxo';
import { LOVELACES_PER_ADA } from '../../config/numbersConfig';
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
    const { app, wallets, walletSettings } = this.props.stores;
    const { walletUtxos } = walletSettings;

    const { active: activeWallet } = wallets;
    if (!activeWallet)
      throw new Error('Active wallet required for WalletUtxoPage.');

    const histogram = get(walletUtxos, 'distribution', {});
    const chartData = getUtxoChartData(histogram);
    const walletUtxosAmount = getWalletUtxosTotalAmount(histogram);
    // Quantity in lowelace
    const walletDistributionAmount = get(walletUtxos, ['total', 'quantity'], 0);
    const formattedWalletDistributionAmount = new BigNumber(
      walletDistributionAmount
    ).dividedBy(LOVELACES_PER_ADA);

    return (
      <WalletUtxo
        walletDistributionAmount={formattedWalletDistributionAmount}
        walletUtxosAmount={walletUtxosAmount}
        chartData={chartData}
        onExternalLinkClick={app.openExternalLink}
      />
    );
  }
}
