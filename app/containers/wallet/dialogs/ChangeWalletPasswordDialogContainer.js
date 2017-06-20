// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import ChangeWalletPasswordDialog from '../../../components/wallet/settings/ChangeWalletPasswordDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('actions', 'stores') @observer
export default class ChangeWalletPasswordDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null };

  props: InjectedProps;

  resetErrors = () => {
    const {
      changeWalletPasswordRequest,
      setWalletPasswordRequest,
    } = this.props.stores.walletSettings;
    changeWalletPasswordRequest.reset();
    setWalletPasswordRequest.reset();
  };

  render() {
    const { actions } = this.props;
    const { wallets, walletSettings, uiDialogs } = this.props.stores;
    const dialogData = uiDialogs.dataForActiveDialog;
    const { updateDataForActiveDialog } = actions.dialogs;
    const activeWallet = wallets.active;
    const {
      changeWalletPasswordRequest,
      setWalletPasswordRequest,
    } = walletSettings;

    if (!activeWallet) throw new Error('Active wallet required for ChangeWalletPasswordDialogContainer.');

    return (
      <ChangeWalletPasswordDialog
        isWalletPasswordSet={activeWallet.hasPassword}
        currentPasswordValue={dialogData.currentPasswordValue}
        newPasswordValue={dialogData.newPasswordValue}
        repeatedPasswordValue={dialogData.repeatedPasswordValue}
        onSave={(values: { oldPassword: string, newPassword: string }) => {
          const walletId = activeWallet.id;
          if (!activeWallet.hasPassword) {
            actions.walletSettings.setWalletPassword.trigger({
              walletId, password: values.newPassword
            });
          } else {
            actions.walletSettings.changeWalletPassword.trigger({
              walletId, oldPassword: values.oldPassword, newPassword: values.newPassword
            });
          }
        }}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
          this.resetErrors();
        }}
        onPasswordSwitchToggle={this.resetErrors}
        onDataChange={data => {
          updateDataForActiveDialog.trigger({ data });
        }}
        isSubmitting={
          changeWalletPasswordRequest.isExecuting || setWalletPasswordRequest.isExecuting
        }
        error={
          changeWalletPasswordRequest.error || setWalletPasswordRequest.error
        }
      />
    );
  }

}
