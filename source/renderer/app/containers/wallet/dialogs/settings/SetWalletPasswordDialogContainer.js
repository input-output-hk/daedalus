// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import SetWalletPasswordDialog from '../../../../components/wallet/settings/SetWalletPasswordDialog';
import type { StoresMap } from '../../../../stores/index';
import type { ActionsMap } from '../../../../actions/index';
import ChangeSpendingPasswordDialog from "../../../../components/wallet/settings/ChangeSpendingPasswordDialog";

type Props = {
  stores: any | StoresMap,
  actions: any | ActionsMap,
};

@inject('actions', 'stores')
@observer
export default class SetWalletPasswordDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  render() {
    const { actions } = this.props;
    const { wallets } = this.props.stores;
    const activeWallet = wallets.active;

    if (!activeWallet)
      throw new Error(
        'Active wallet required for SetWalletPasswordDialogContainer.'
      );

    return (
      <SetWalletPasswordDialog
        onConfirm={() => {
          actions.dialogs.closeActiveDialog.trigger();
          actions.dialogs.open.trigger({
            dialog: ChangeSpendingPasswordDialog,
          })
        }}
      />
    );
  }
}
