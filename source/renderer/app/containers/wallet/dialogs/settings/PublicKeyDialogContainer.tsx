import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import type { InjectedProps } from '../../../../types/injectedPropsType';
import WalletPublicKeyDialog from '../../../../components/wallet/settings/WalletPublicKeyDialog';
import ICOPublicKeyDialog from '../../../../components/wallet/settings/ICOPublicKeyDialog';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
class PublicKeyDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
  };
  handleClose = () => {
    const { actions, stores } = this.props;
    const { accountPublicKeyRequest, icoPublicKeyRequest } = stores.wallets;
    actions.dialogs.closeActiveDialog.trigger();
    accountPublicKeyRequest.reset();
    icoPublicKeyRequest.reset();
  };

  render() {
    const { actions, stores, isICO = false } = this.props;
    const { getAccountPublicKey, getICOPublicKey } = actions.wallets;
    const {
      accountPublicKeyRequest,
      activePublicKey,
      icoPublicKeyRequest,
      icoPublicKey,
      active: activeWallet,
    } = stores.wallets;
    if (!activeWallet) throw new Error('Active wallet required.');
    return isICO ? (
      <ICOPublicKeyDialog
        onRevealPublicKey={getICOPublicKey.trigger}
        onClose={this.handleClose}
        hasReceivedICOPublicKey={!!icoPublicKey}
        error={icoPublicKeyRequest.error}
        walletName={activeWallet.name}
      />
    ) : (
      <WalletPublicKeyDialog
        onRevealPublicKey={getAccountPublicKey.trigger}
        onClose={this.handleClose}
        hasReceivedWalletPublicKey={!!activePublicKey}
        error={accountPublicKeyRequest.error}
        walletName={activeWallet.name}
      />
    );
  }
}

export default PublicKeyDialogContainer;
