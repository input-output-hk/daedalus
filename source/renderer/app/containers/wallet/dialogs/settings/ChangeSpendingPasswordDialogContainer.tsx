import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ChangeSpendingPasswordDialog from '../../../../components/wallet/settings/ChangeSpendingPasswordDialog';
import type { StoresMap } from '../../../../stores/index';
import type { ActionsMap } from '../../../../actions/index';

type Props = {
  stores: any | StoresMap;
  actions: any | ActionsMap;
};

@inject('actions', 'stores')
@observer
class ChangeSpendingPasswordDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { actions } = this.props;
    const { uiDialogs, wallets, walletSettings, profile } = this.props.stores;
    const { currentLocale } = profile;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const { updateSpendingPasswordRequest } = walletSettings;
    if (!activeWallet)
      throw new Error(
        'Active wallet required for ChangeSpendingPasswordDialogContainer.'
      );
    return (
      <ChangeSpendingPasswordDialog
        isSpendingPasswordSet={activeWallet.hasPassword}
        currentPasswordValue={dialogData.currentPasswordValue}
        newPasswordValue={dialogData.newPasswordValue}
        repeatedPasswordValue={dialogData.repeatedPasswordValue}
        onSave={(values: { oldPassword: string; newPassword: string }) => {
          const { id: walletId, isLegacy } = activeWallet;
          const { oldPassword, newPassword } = values;
          actions.walletSettings.updateSpendingPassword.trigger({
            walletId,
            oldPassword,
            newPassword,
            isLegacy,
          });
        }}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          updateSpendingPasswordRequest.reset();
        }}
        onDataChange={(data) => {
          updateDataForActiveDialog.trigger({
            data,
          });
        }}
        isSubmitting={updateSpendingPasswordRequest.isExecuting}
        error={updateSpendingPasswordRequest.error}
        walletName={activeWallet.name}
        currentLocale={currentLocale}
      />
    );
  }
}

export default ChangeSpendingPasswordDialogContainer;
