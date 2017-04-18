// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import type { AssuranceMode } from '../../types/transactionAssuranceTypes';

@inject('stores', 'actions') @observer
export default class WalletSettingsPage extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  handleWalletAssuranceLevelUpdate = (values: { assurance: AssuranceMode }) => {
    this.props.actions.walletSettings.updateWalletAssuranceLevel.trigger(values);
  };

  render() {
    const { wallets, walletSettings, uiDialogs } = this.props.stores;
    const { actions } = this.props;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSettingsPage.');

    return (
      <WalletSettings
        assuranceLevels={walletSettings.WALLET_ASSURANCE_LEVEL_OPTIONS}
        walletAssurance={activeWallet.assurance}
        onWalletAssuranceLevelUpdate={this.handleWalletAssuranceLevelUpdate}
        error={walletSettings.updateWalletRequest.error}
        openDialogAction={actions.dialogs.open.trigger}
        isDialogOpen={uiDialogs.isOpen}
      />
    );
  }

}
