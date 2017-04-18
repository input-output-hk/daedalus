// @flow
import React, { Component, PropTypes } from 'react';
import { inject, observer } from 'mobx-react';
import UiDialogsStore from '../../../stores/UiDialogsStore';
import WalletsStore from '../../../stores/WalletsStore';
import ChangeWalletPasswordDialog from '../../../components/wallet/settings/ChangeWalletPasswordDialog';

@inject('actions', 'stores') @observer
export default class ChangeWalletPasswordDialogContainer extends Component {

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
      walletSettings: PropTypes.shape({
        changeWalletPassword: PropTypes.func.isRequired,
        setWalletPassword: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
  };

  render() {
    const { actions } = this.props;
    const { wallets, uiDialogs } = this.props.stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    return (
      <ChangeWalletPasswordDialog
        hasWalletPassword={wallets.active.hasPassword}
        currentPasswordValue={dialogData.currentPasswordValue}
        newPasswordValue={dialogData.newPasswordValue}
        repeatedPasswordValue={dialogData.repeatedPasswordValue}
        onSave={(values: { oldPassword: string, newPassword: string }) => {
          const walletId = wallets.active.id;
          if (!wallets.active.hasPassword) {
            actions.walletSettings.setWalletPassword({
              walletId, password: values.newPassword
            });
          } else {
            actions.walletSettings.changeWalletPassword({
              walletId, oldPassword: values.oldPassword, newPassword: values.newPassword
            });
          }
          actions.dialogs.resetActiveDialog();
        }}
        onCancel={actions.dialogs.closeActiveDialog}
        onDataChange={data => {
          updateDataForActiveDialog({ data });
        }}
      />
    );
  }

}
