// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ChangeSpendingPasswordDialog from '../../../../components/wallet/settings/ChangeSpendingPasswordDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

@inject('actions', 'stores')
@observer
export default class ChangeSpendingPasswordDialogContainer extends Component<InjectedProps> {
  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions } = this.props;
    const { uiDialogs, wallets, walletSettings } = this.props.stores;
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
        onSave={(values: { oldPassword: string, newPassword: string }) => {
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
        onDataChange={data => {
          updateDataForActiveDialog.trigger({ data });
        }}
        isSubmitting={updateSpendingPasswordRequest.isExecuting}
        error={updateSpendingPasswordRequest.error}
      />
    );
  }
}
