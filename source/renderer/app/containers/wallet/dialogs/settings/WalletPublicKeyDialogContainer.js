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
    actions.dialogs.closeActiveDialog.trigger();
    stores.wallets.accountPublicKeyRequest.reset();
  };

  render() {
    const { actions, stores } = this.props;
    const { getAccountPublicKey } = actions.wallets;
    const {
      walletPublicKeyRequest,
      walletsPublicKeys,
      active,
    } = stores.wallets;
    if (!active) {
      return null;
    }
    return (
      <WalletPublicKeyDialog
        onRevealPublicKey={getAccountPublicKey.trigger}
        onClose={this.handleClose}
        hasReceivedWalletPublicKey={!!walletsPublicKeys[active.id]}
        error={walletPublicKeyRequest.error}
      />
    );
  }
}
