// @flow
import React, { Component, PropTypes } from 'react';
import { observer, inject, PropTypes as MobxPropTypes } from 'mobx-react';
import WalletAddDialog from '../../components/wallet/WalletAddDialog';

@inject('stores', 'actions') @observer
export default class WalletAddPage extends Component {

  static propTypes = {
    actions: PropTypes.shape({
      toggleCreateWalletDialog: PropTypes.func.isRequired,
      toggleWalletImport: PropTypes.func.isRequired,
      toggleAddWallet: PropTypes.func.isRequired,
    }).isRequired
  };

  render() {
    const {
      toggleCreateWalletDialog,
      toggleWalletImport,
      toggleAddWallet
    } = this.props.actions;
    return (
    <WalletAddDialog
      onCreate={toggleCreateWalletDialog}
      onImport={toggleWalletImport}
      onCancel={toggleAddWallet}
    />
    );
  }

}
