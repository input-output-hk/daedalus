// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Request from '../../stores/lib/Request';
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
        updateWalletUnitRequest: PropTypes.instanceOf(Request).isRequired,
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
    const { updateWalletUnitRequest, WALLET_UNIT_OPTIONS } = walletSettings;
    return (
      <WalletSettings
        walletUnit={wallet.unit}
        onWalletUnitUpdate={this.handleWalletUnitUpdate}
        isWalletUnitUpdated={
          updateWalletUnitRequest.wasExecuted && updateWalletUnitRequest.result != null
        }
        units={WALLET_UNIT_OPTIONS}
        error={updateWalletUnitRequest.error}
      />
    );
  }

}
