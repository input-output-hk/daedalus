// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import WalletPublicKeyDialog from '../../../../components/wallet/settings/WalletPublicKeyDialog';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class WalletPublicKeyDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleClose = () => {
    const { actions, stores } = this.props;
    const { accountPublicKeyRequest } = stores.wallets;
    actions.dialogs.closeActiveDialog.trigger();
    accountPublicKeyRequest.reset();
  };

  render() {
    const { actions, stores } = this.props;
    const { getAccountPublicKey } = actions.wallets;
    const { accountPublicKeyRequest, activePublicKey } = stores.wallets;

    return (
      <WalletPublicKeyDialog
        onRevealPublicKey={getAccountPublicKey.trigger}
        onClose={this.handleClose}
        hasReceivedWalletPublicKey={!!activePublicKey}
        error={accountPublicKeyRequest.error}
      />
    );
  }
}
