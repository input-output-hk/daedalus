// @flow
// TODO: Remove once the new wallet creation process is ready
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletCreateDialog from '../../../components/wallet/WalletCreateDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class WalletCreateDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onSubmit = (values: { name: string, spendingPassword: string }) => {
    this.props.actions.wallets.createWallet.trigger(values);
  };

  render() {
    return (
      <WalletCreateDialog
        onSubmit={this.onSubmit}
        onCancel={this.props.onClose}
      />
    );
  }
}
