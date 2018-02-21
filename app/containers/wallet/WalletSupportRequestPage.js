// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import BugReportDialog from '../../components/profile/bug-report/BugReportDialog';
import BugReportDialogContainer from '../profile/dialogs/BugReportDialogContainer';
import type { InjectedProps } from '../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSupportRequestPage extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { uiDialogs } = this.props.stores;
    let activeDialog = null;
    if (uiDialogs.isOpen(BugReportDialog)) {
      activeDialog = <BugReportDialogContainer />;
    }

    return activeDialog;
  }

}
