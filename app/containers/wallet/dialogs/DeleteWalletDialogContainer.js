// @flow
import React, { Component, PropTypes } from 'react';
import { inject, observer } from 'mobx-react';
import UiDialogsStore from '../../../stores/UiDialogsStore';
import WalletsStore from '../../../stores/WalletsStore';
import DeleteWalletConfirmationDialog from '../../../components/wallet/settings/DeleteWalletConfirmationDialog';

@inject('actions', 'stores') @observer
export default class DeleteWalletDialogContainer extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.instanceOf(WalletsStore).isRequired,
      uiDialogs: PropTypes.instanceOf(UiDialogsStore).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      dialogs: PropTypes.shape({
        open: PropTypes.func.isRequired,
        closeActiveDialog: PropTypes.func.isRequired,
        resetActiveDialog: PropTypes.func.isRequired,
        updateDataForActiveDialog: PropTypes.func.isRequired,
      }).isRequired,
      wallets: PropTypes.shape({
        delete: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
  };

  render() {
    const { actions } = this.props;
    const { wallets, uiDialogs } = this.props.stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
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
          actions.wallets.delete({ walletId: wallets.active.id });
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
