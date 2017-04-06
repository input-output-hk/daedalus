// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DeleteWalletConfirmationDialog from '../../../components/wallet/settings/DeleteWalletConfirmationDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('actions', 'stores') @observer
export default class DeleteWalletDialogContainer extends Component {

  props: InjectedProps;

  render() {
    const { actions } = this.props;
    const { wallets, uiDialogs } = this.props.stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    updateDataForActiveDialog();
    return (
      <DeleteWalletConfirmationDialog
        walletName={wallets.active.name}
        hasWalletFunds={wallets.active.hasFunds}
        countdownFn={uiDialogs.countdownSinceDialogOpened}
        isBackupNoticeAccepted={dialogData.isBackupNoticeAccepted}
        onAcceptBackupNotice={() => updateDataForActiveDialog({
          data: { isBackupNoticeAccepted: true }
        })}
        onContinue={() => {
          actions.wallets.deleteWallet.trigger({ walletId: wallets.active.id });
          actions.dialogs.resetActiveDialog();
        }}
        onCancel={actions.dialogs.closeActiveDialog}
        confirmationValue={dialogData.confirmationValue}
        onConfirmationValueChange={confirmationValue => updateDataForActiveDialog({
          data: { confirmationValue }
        })}
      />
    );
  }

}
