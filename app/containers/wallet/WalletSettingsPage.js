// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Wallet from '../../domain/Wallet';
import WalletSettings from '../../components/wallet/WalletSettings';

@inject('stores', 'actions') @observer
export default class WalletSettingsPage extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        active: PropTypes.instanceOf(Wallet),
      }),
      walletSettings: PropTypes.shape({
        WALLET_UNIT_OPTIONS: PropTypes.array.isRequired,
      }),
    }),
    actions: PropTypes.shape({
      walletSettings: PropTypes.shape({
        updateWalletUnit: PropTypes.func.isRequired,
      }),
    }),
  };

  handleWalletUnitUpdate = (values: { unit: number }) => {
    this.props.actions.walletSettings.updateWalletUnit(values);
  };

  render() {
    const { wallets, walletSettings } = this.props.stores;
    const wallet = wallets.active;
    const WALLET_UNIT_OPTIONS = walletSettings.WALLET_UNIT_OPTIONS;
    return (
      <WalletSettings
        walletUnit={wallet.unit}
        onWalletUnitUpdate={this.handleWalletUnitUpdate}
        units={WALLET_UNIT_OPTIONS}
      />
    );
  }

}
