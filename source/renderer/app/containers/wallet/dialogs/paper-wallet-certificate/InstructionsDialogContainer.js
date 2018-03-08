// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import type { ActionsMap } from '../../../../actions/index';

type Props = {
  actions: any | ActionsMap,
  onClose: Function,
};

@inject('actions') @observer
export default class InstructionsDialogContainer extends Component<Props> {

  static defaultProps = { actions: null, stores: null };

  onContinue = () => {
    this.props.actions.ada.wallets.updateCertificateStep.trigger();
  }

  render() {
    return (
      <InstructionsDialog
        onContinue={this.onContinue}
        onClose={this.props.onClose}
      />
    );
  }
}
