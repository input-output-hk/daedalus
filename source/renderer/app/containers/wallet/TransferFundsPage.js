// @flow
import React, { Component } from 'react';
import { inject, observer } from 'mobx-react';
import TransferFundsStep1Container from './dialogs/transfer-funds/TransferFundsStep1Container';
import TransferFundsStep2Container from './dialogs/transfer-funds/TransferFundsStep2Container';
import type { InjectedProps } from '../../types/injectedPropsType';

type Props = InjectedProps;

@inject('actions', 'stores')
@observer
export default class PaperWalletCreateCertificatePage extends Component<Props> {
  static defaultProps = { actions: null, stores: null };

  handleClose = () => {
    console.log('ON CLOSE');
    // ...
  };

  handleContinue = () => {
    console.log('ON CONTINUE');
    // ...
  };

  render() {
    const { transferFundsStep } = this.props.stores.wallets;
    if (transferFundsStep === null) return null;
    let Container = null;
    if (transferFundsStep === 0) {
      Container = TransferFundsStep1Container;
    }
    if (transferFundsStep === 1) {
      Container = TransferFundsStep2Container;
    }
    return (
      <Container onClose={this.handleClose} onContinue={this.handleContinue} />
    );
  }
}
