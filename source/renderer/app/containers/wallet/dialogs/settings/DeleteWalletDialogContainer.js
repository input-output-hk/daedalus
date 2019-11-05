// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import DeleteWalletConfirmationDialog from '../../../../components/wallet/settings/DeleteWalletConfirmationDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class DeleteWalletDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions, stores } = this.props;
    const { uiDialogs, wallets, app } = stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const { deleteWalletRequest } = wallets;
    const {
      environment: { isTest },
    } = app;

    // Guard against potential null values
    if (!activeWallet)
      throw new Error(
        'Active wallet required for DeleteWalletDialogContainer.'
      );

    return (
      <DeleteWalletConfirmationDialog
        isTest={isTest}
        walletName={activeWallet.name}
        hasWalletFunds={activeWallet.hasFunds}
        countdownFn={uiDialogs.countdownSinceDialogOpened}
        isBackupNoticeAccepted={dialogData.isBackupNoticeAccepted}
        onAcceptBackupNotice={() =>
          updateDataForActiveDialog.trigger({
            data: { isBackupNoticeAccepted: true },
          })
        }
        onContinue={() => {
          actions.wallets.deleteWallet.trigger({
            walletId: activeWallet.id,
            isLegacy: activeWallet.isLegacy,
          });
        }}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          deleteWalletRequest.reset();
        }}
        confirmationValue={dialogData.confirmationValue}
        onConfirmationValueChange={confirmationValue =>
          updateDataForActiveDialog.trigger({
            data: { confirmationValue },
          })
        }
        isSubmitting={deleteWalletRequest.isExecuting}
      />
    );
  }
}
