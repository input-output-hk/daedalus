import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SuccessDialog from '../../../../components/wallet/wallet-restore/SuccessDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
class SuccessDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, actions } = this.props;
    const { walletKindDaedalus, walletKindYoroi } = stores.wallets;
    const { restoreWalletEnd } = actions.wallets;
    return (
      <SuccessDialog
        onClose={() => restoreWalletEnd.trigger()}
        walletKindDaedalus={walletKindDaedalus}
        walletKindYoroi={walletKindYoroi}
      />
    );
  }
}

export default SuccessDialogContainer;
