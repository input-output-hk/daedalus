// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import PrintDialog from '../../../../components/wallet/paper-wallet-certificate/PrintDialog';
import type { InjectedProps } from '../../../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions') @observer
export default class PrintDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  onContinue = () => {
    this.props.actions.ada.wallets.updateCertificateStep.trigger();
  };

  render() {
    return (
      <PrintDialog
        onContinue={this.onContinue}
      />
    );
  }
}
