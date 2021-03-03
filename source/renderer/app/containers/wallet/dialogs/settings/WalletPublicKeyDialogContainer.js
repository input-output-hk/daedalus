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

  render() {
    const { actions } = this.props;
    const { revealPublicKey } = actions.wallets;
    return (
      <WalletPublicKeyDialog
        onRevealPublicKey={revealPublicKey.trigger}
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
        error={null}
      />
    );
  }
}
