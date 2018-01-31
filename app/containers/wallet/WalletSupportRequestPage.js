// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletBugReportDialog from '../../components/wallet/WalletBugReportDialog';
import WalletBugReportDialogContainer from '../wallet/dialogs/WalletBugReportDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSupportRequestPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs } = this.props.stores;
    let activeDialog = null;
    if (uiDialogs.isOpen(WalletBugReportDialog)) {
      activeDialog = <WalletBugReportDialogContainer />;
    }

    return activeDialog;
  }

}
