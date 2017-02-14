// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';

@inject('stores', 'actions') @observer
export default class WalletAddPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      toggleCreateWalletDialog: PropTypes.func.isRequired,
      toggleWalletRestore: PropTypes.func.isRequired,
      toggleAddWallet: PropTypes.func.isRequired,
    }).isRequired
  };

  render() {
    const {
      toggleCreateWalletDialog,
      toggleWalletRestore,
      toggleAddWallet
    } = this.props.actions;
    return (
      <WalletAddDialog
        onCreate={toggleCreateWalletDialog}
        onRestore={toggleWalletRestore}
        onCancel={toggleAddWallet}
      />
    );
  }

}
