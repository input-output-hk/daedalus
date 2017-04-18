// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DeleteWalletConfirmationDialog from '../../../components/wallet/settings/DeleteWalletConfirmationDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('actions', 'stores') @observer
export default class DeleteWalletDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null };
  props: InjectedProps;

  render() {
    const { actions } = this.props;
    const { wallets, uiDialogs } = this.props.stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;

    // Guard against potential null values
    if (!activeWallet) throw new Error('Active wallet required for DeleteWalletDialogContainer.');

    return (
      <DeleteWalletConfirmationDialog
        walletName={activeWallet.name}
        hasWalletFunds={activeWallet.hasFunds}
        countdownFn={uiDialogs.countdownSinceDialogOpened}
        isBackupNoticeAccepted={dialogData.isBackupNoticeAccepted}
        onAcceptBackupNotice={() => updateDataForActiveDialog.trigger({
          data: { isBackupNoticeAccepted: true }
        })}
        onContinue={() => {
          actions.wallets.deleteWallet.trigger({ walletId: activeWallet.id });
          actions.dialogs.resetActiveDialog.trigger();
        }}
        onCancel={actions.dialogs.closeActiveDialog.trigger}
        confirmationValue={dialogData.confirmationValue}
        onConfirmationValueChange={confirmationValue => updateDataForActiveDialog.trigger({
          data: { confirmationValue }
        })}
      />
    );
  }

}
