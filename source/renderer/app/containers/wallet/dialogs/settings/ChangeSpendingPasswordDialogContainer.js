// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ChangeSpendingPasswordDialog from '../../../../components/wallet/settings/ChangeSpendingPasswordDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import environment from '../../../../../../common/environment';

@inject('actions', 'stores') @observer
export default class ChangeSpendingPasswordDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions } = this.props;
    const { uiDialogs } = this.props.stores;
    const { wallets, walletSettings } = this.props.stores[environment.API];
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const { updateSpendingPasswordRequest } = walletSettings;

    if (!activeWallet) throw new Error('Active wallet required for ChangeSpendingPasswordDialogContainer.');

    return (
      <ChangeSpendingPasswordDialog
        isSpendingPasswordSet={activeWallet.hasPassword}
        currentPasswordValue={dialogData.currentPasswordValue}
        newPasswordValue={dialogData.newPasswordValue}
        repeatedPasswordValue={dialogData.repeatedPasswordValue}
        onSave={(values: { oldPassword: string, newPassword: string }) => {
          const walletId = activeWallet.id;
          const { oldPassword, newPassword } = values;
          actions[environment.API].walletSettings.updateSpendingPassword.trigger({
            walletId, oldPassword, newPassword
          });
        }}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          updateSpendingPasswordRequest.reset();
        }}
        onPasswordSwitchToggle={() => {
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
