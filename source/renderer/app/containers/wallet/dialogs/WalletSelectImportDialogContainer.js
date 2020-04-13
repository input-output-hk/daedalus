// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';
import WalletSelectImportDialog from '../../../components/wallet/wallet-import/WalletSelectImportDialog';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletSelectImportDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onConfirm = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
  };

  onCancel = () => {
    this.props.actions.dialogs.closeActiveDialog.trigger();
    const { importFromFileRequest } = this.props.stores.wallets;
    if (!importFromFileRequest.isExecuting) importFromFileRequest.reset();
  };

  render() {
    return (
      <WalletSelectImportDialog
        onConfirm={this.onConfirm}
        onClose={this.onCancel}
      />
    );
  }
}
