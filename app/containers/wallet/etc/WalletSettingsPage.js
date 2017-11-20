// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSettings from '../../../components/wallet/etc/WalletSettings';
import type { InjectedProps } from '../../../types/injectedPropsType';
import { isValidWalletName } from '../../../utils/validations';

type Props = InjectedProps

@inject('stores', 'actions') @observer
export default class WalletSettingsPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs } = this.props.stores;
    const { wallets, walletSettings } = this.props.stores.etc;
    const { actions } = this.props;
    const activeWallet = wallets.active;
    const {
      updateWalletRequest,
      lastUpdatedWalletField,
      walletFieldBeingEdited,
    } = walletSettings;
    const {
      startEditingWalletField,
      stopEditingWalletField,
      cancelEditingWalletField,
      updateWalletField,
    } = actions.etc.walletSettings;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for WalletSettingsPage.');

    return (
      <WalletSettings
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
