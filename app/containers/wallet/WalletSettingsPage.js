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
        updateWalletRequest: PropTypes.instanceOf(Request).isRequired,
        WALLET_ASSURANCE_LEVEL_OPTIONS: PropTypes.array.isRequired,
        WALLET_UNIT_OPTIONS: PropTypes.array.isRequired,
      }),
    }),
    actions: PropTypes.shape({
      walletSettings: PropTypes.shape({
        updateWalletAssuranceLevel: PropTypes.func.isRequired,
        updateWalletUnit: PropTypes.func.isRequired,
      }),
    }),
  };

  handleWalletAssuranceLevelUpdate = (values: { assurance: string }) => {
    this.props.actions.walletSettings.updateWalletAssuranceLevel(values);
  };

  handleWalletUnitUpdate = (values: { unit: number }) => {
    this.props.actions.walletSettings.updateWalletUnit(values);
  };

  render() {
    const { wallets, walletSettings } = this.props.stores;
    const wallet = wallets.active;
    const {
      updateWalletRequest,
      WALLET_ASSURANCE_LEVEL_OPTIONS,
      WALLET_UNIT_OPTIONS,
    } = walletSettings;
    return (
      <WalletSettings
        assuranceLevels={WALLET_ASSURANCE_LEVEL_OPTIONS}
        walletAssurance={wallet.assurance}
        walletUnit={wallet.unit}
        onWalletAssuranceLevelUpdate={this.handleWalletAssuranceLevelUpdate}
        onWalletUnitUpdate={this.handleWalletUnitUpdate}
        units={WALLET_UNIT_OPTIONS}
        error={updateWalletRequest.error}
      />
    );
  }

}
