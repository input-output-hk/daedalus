// @flow
import React, { Component } from 'react';
import { remote } from 'electron';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions') @observer
export default class InstructionsDialogContainer extends Component<Props> {
  static defaultProps = { actions: null, stores: null, children: null, onClose: () => {} };

  onPrint = () => {
    const filePath = remote.dialog.showSaveDialog({
      defaultPath: 'paper-wallet-certificate.pdf',
      filters: [{
        name: 'paper-wallet-certificate',
        extensions: ['pdf'],
      }],
    });

    // if cancel button is clicked or path is empty
    if (!filePath) return;

    this.props.actions.wallets.generateCertificate.trigger({ filePath });
  };

  render() {
    const { wallets, app } = this.props.stores;

    return (
      <InstructionsDialog
        inProgress={wallets.generatingCertificateInProgress}
        network={app.environment.NETWORK}
        onPrint={this.onPrint}
        onClose={this.props.onClose}
        onOpenExternalLink={this.props.stores.app.openExternalLink}
      />
    );
  }
}
