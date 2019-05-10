// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class InstructionsDialogContainer extends Component<Props> {
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onPrint = () => {
    let filePath;

    try {
      // TODO: refactor this direct access to the dialog api
      filePath = global.dialog.showSaveDialog({
        defaultPath: 'paper-wallet-certificate.pdf',
        filters: [
          {
            name: 'paper-wallet-certificate',
            extensions: ['pdf'],
          },
        ],
      });
    } catch (error) {
      if (error && error.syscall && error.syscall === 'open') {
        alert(
          'Please, close the existing PDF file or save with a different name'
        ); // eslint-disable-line
      }
      return false;
    }

    // if cancel button is clicked or path is empty
    if (!filePath) return false;

    return this.props.actions.wallets.generateCertificate.trigger({ filePath });
  };

  render() {
    const { wallets, app } = this.props.stores;
    const {
      openExternalLink,
      environment: { network },
    } = app;
    return (
      <InstructionsDialog
        inProgress={wallets.generatingCertificateInProgress}
        network={network}
        onPrint={this.onPrint}
        onClose={this.props.onClose}
        onOpenExternalLink={openExternalLink}
      />
    );
  }
}
