// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import WalletSupportRequestDialog from '../../../components/wallet/WalletSupportRequestDialog';
import type { InjectedProps } from '../../../types/injectedPropsType';

@inject('stores', 'actions') @observer
export default class WalletSupportRequestDialogContainer extends Component<InjectedProps> {

  static defaultProps = { actions: null, stores: null };

  render() {
    const { actions } = this.props;

    return (
      <WalletSupportRequestDialog
        onCancel={() => {
          actions.dialogs.closeActiveDialog.trigger();
        }}
        onSave={() => {
          // TODO - implement api call to send support request
          actions.dialogs.closeActiveDialog.trigger();
        }}
        isSubmitting={false}
        error={null}
      />
    );
  }
}
