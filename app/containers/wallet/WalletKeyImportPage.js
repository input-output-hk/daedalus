// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletKeyImportDialog from '../../components/wallet/key-import/WalletKeyImportDialog';
import Request from '../../stores/lib/Request';

@inject('stores', 'actions') @observer
export default class WalletKeyImportPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      importWalletFromKey: PropTypes.func.isRequired,
      toggleWalletKeyImportDialog: PropTypes.func.isRequired,
    }),
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        importFromKeyRequest: PropTypes.instanceOf(Request).isRequired,
      }).isRequired,
    }).isRequired
  };

  onSubmit = (values) => {
    this.props.actions.importWalletFromKey(values);
  };

  render() {
    const { wallets } = this.props.stores;
    const { importFromKeyRequest } = wallets;
    const { toggleWalletKeyImportDialog } = this.props.actions;

    return (
      <WalletKeyImportDialog
        isSubmitting={importFromKeyRequest.isExecuting}
        onSubmit={this.onSubmit}
        error={importFromKeyRequest.error}
        onClose={toggleWalletKeyImportDialog}
      />
    );
  }
}
