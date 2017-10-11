// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletCreateDialog from '../../../components/wallet/WalletCreateDialog';
import type { InjectedDialogContainerProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletCreateDialogContainer extends Component {

  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  props: InjectedDialogContainerProps;

  onSubmit = (values: { name: string, password: ?string }) => {
    this.props.actions.ada.wallets.createWallet.trigger(values);
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
