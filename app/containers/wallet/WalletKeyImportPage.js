// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletKeyImportDialog from '../../components/wallet/key-import/WalletKeyImportDialog';
import Request from '../../stores/lib/Request';

@inject('stores', 'actions') @observer
export default class WalletKeyImportPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      importWalletKey: PropTypes.func.isRequired,
      toggleWalletKeyImportDialog: PropTypes.func.isRequired,
    }),
    stores: PropTypes.shape({
      wallets: PropTypes.shape({
        importKeyRequest: PropTypes.instanceOf(Request).isRequired,
        keyFile: PropTypes.instanceOf(File),
        error: PropTypes.instanceOf(Error),
      }).isRequired,
    }).isRequired
  };

  onSubmit = (values) => {
    this.props.actions.importWalletKey(values);
  };

  render() {
    const { wallets } = this.props.stores;
    const { importKeyRequest, keyFile, error } = wallets;
    const { toggleWalletKeyImportDialog, setKeyFile } = this.props.actions;

    return (
      <WalletKeyImportDialog
        onKeyFileSelected={(keyFile) => setKeyFile({ keyFile })}
        isKeyFileSelected={keyFile !== null}
        isSubmitting={importKeyRequest.isExecuting}
        onSubmit={this.onSubmit}
        error={error}
        onClose={toggleWalletKeyImportDialog}
      />
    );
  }
}
