// @flow
import React, { Component, PropTypes } from 'react';
import { inject, observer } from 'mobx-react';
import UiDialogsStore from '../../../stores/UiDialogsStore';
import WalletsStore from '../../../stores/WalletsStore';
import ChangeWalletPasswordDialog from '../../../components/wallet/settings/ChangeWalletPasswordDialog';

@inject('actions', 'stores') @observer
export default class ChangeWalletPasswordDialogContainer extends Component {

  static propTypes = {
    stores: PropTypes.shape({
      wallets: PropTypes.instanceOf(WalletsStore).isRequired,
      uiDialogs: PropTypes.instanceOf(UiDialogsStore).isRequired,
    }).isRequired,
    actions: PropTypes.shape({
      dialogs: PropTypes.shape({
        open: PropTypes.func.isRequired,
        closeActiveDialog: PropTypes.func.isRequired,
        resetActiveDialog: PropTypes.func.isRequired,
        updateDataForActiveDialog: PropTypes.func.isRequired,
      }).isRequired,
    }).isRequired,
  };

  render() {
    const { actions } = this.props;
    const { wallets } = this.props.stores;
    return (
      <ChangeWalletPasswordDialog
        hasWalletPassword={wallets.active.hasPassword}
        onSave={() => {
          actions.dialogs.resetActiveDialog();
        }}
        onCancel={actions.dialogs.closeActiveDialog}
      />
    );
  }

}
