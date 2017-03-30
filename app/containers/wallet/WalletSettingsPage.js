// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import Request from '../../stores/lib/Request';
import Wallet from '../../domain/Wallet';
import WalletSettings from '../../components/wallet/WalletSettings';
import UiDialogsStore from '../../stores/UiDialogsStore';

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
      }),
      uiDialogs: PropTypes.instanceOf(UiDialogsStore).isRequired,
    }),
    actions: PropTypes.shape({
      walletSettings: PropTypes.shape({
        updateWalletAssuranceLevel: PropTypes.func.isRequired,
      }).isRequired,
      dialogs: PropTypes.shape({
        open: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
  };

  handleWalletAssuranceLevelUpdate = (values: { assurance: string }) => {
    this.props.actions.walletSettings.updateWalletAssuranceLevel(values);
  };

  render() {
    const { wallets, walletSettings, uiDialogs } = this.props.stores;
    const { actions } = this.props;
    const wallet = wallets.active;
    const {
      updateWalletRequest,
      WALLET_ASSURANCE_LEVEL_OPTIONS,
    } = walletSettings;
    return (
      <WalletSettings
        assuranceLevels={WALLET_ASSURANCE_LEVEL_OPTIONS}
        walletAssurance={wallet.assurance}
        walletUnit={wallet.unit}
        onWalletAssuranceLevelUpdate={this.handleWalletAssuranceLevelUpdate}
        error={updateWalletRequest.error}
        openDialogAction={actions.dialogs.open}
        isDialogOpen={uiDialogs.isOpen}
      />
    );
  }

}
