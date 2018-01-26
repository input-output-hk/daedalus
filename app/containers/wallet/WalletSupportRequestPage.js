// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSupportRequestDialog from '../../components/wallet/WalletSupportRequestDialog';
import WalletSupportRequestDialogContainer from '../wallet/dialogs/WalletSupportRequestDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('stores', 'actions') @observer
export default class WalletSupportRequestPage extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs } = this.props.stores;
    let activeDialog = null;
    if (uiDialogs.isOpen(WalletSupportRequestDialog)) {
      activeDialog = <WalletSupportRequestDialogContainer />;
    }

    return activeDialog;
  }

}
