// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import SuccessDialog from '../../../../components/wallet/wallet-restore/SuccessDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

@inject('stores', 'actions')
@observer
export default class SuccessDialogContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  render() {
    const { stores, actions } = this.props;
    const { walletKindDaedalus, walletKindYoroi } = stores.wallets;
    const { restoreWalletClose } = actions.wallets;
    return (
      <SuccessDialog
        onClose={() => restoreWalletClose.trigger()}
        walletKindDaedalus={walletKindDaedalus}
        walletKindYoroi={walletKindYoroi}
      />
    );
  }
}
