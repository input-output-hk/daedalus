// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import PaperWalletImportDialog from '../../../components/wallet/PaperWalletImportDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores') @observer
export default class PaperWalletImportDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onSubmit = () => {
    // TODO - call proper method to store paper wallet
    this.props.onClose();
  };

  onCancel = () => {
    this.props.onClose();
  };

  render() {
    const { wallets } = this.props.stores;

    return (
      <PaperWalletImportDialog
        mnemonicValidator={mnemonic => wallets.isValidMnemonic(mnemonic)}
        privateKeyValidator={privateKey => wallets.isValidPrivateKey(privateKey)}
        isSubmitting={false} // TODO - use isExecuting when request would be done
        onSubmit={this.onSubmit}
        onCancel={this.onCancel}
        error={null} // TODO - use request errors when request would be done
      />
    );
  }
}
