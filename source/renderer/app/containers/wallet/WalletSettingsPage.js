// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../components/wallet/WalletSettings';
import type { InjectedProps } from '../../types/injectedPropsType';
import { isValidWalletName } from '../../utils/validations';

type Props = InjectedProps

@inject('stores', 'actions') @observer
export default class WalletSettingsPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs } = this.props.stores;
    const { wallets, walletSettings } = this.props.stores.ada;
    const { actions } = this.props;
    const activeWallet = wallets.active;
    const {
      WALLET_ASSURANCE_LEVEL_OPTIONS,
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
    } = actions.ada.walletSettings;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSettingsPage.');

    return (
      <WalletSettings
        assuranceLevels={WALLET_ASSURANCE_LEVEL_OPTIONS}
        walletAssurance={activeWallet.assurance}
        error={updateWalletRequest.error}
        openDialogAction={actions.dialogs.open.trigger}
        isWalletPasswordSet={activeWallet.hasPassword}
        walletPasswordUpdateDate={activeWallet.passwordUpdateDate}
        isDialogOpen={uiDialogs.isOpen}
        walletName={activeWallet.name}
        isSubmitting={updateWalletRequest.isExecuting}
        isInvalid={updateWalletRequest.wasExecuted && updateWalletRequest.result === false}
        lastUpdatedField={lastUpdatedWalletField}
        onFieldValueChange={(field, value) => updateWalletField.trigger({ field, value })}
        onStartEditing={field => startEditingWalletField.trigger({ field })}
        onStopEditing={stopEditingWalletField.trigger}
        onCancelEditing={cancelEditingWalletField.trigger}
        activeField={walletFieldBeingEdited}
        nameValidator={name => isValidWalletName(name)}
      />
    );
  }

}
