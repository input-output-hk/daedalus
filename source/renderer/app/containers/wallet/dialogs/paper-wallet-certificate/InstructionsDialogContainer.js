// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import InstructionsDialog from '../../../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../../common/utils/files';

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
    // TODO: refactor this direct access to the dialog api
    const name = generateFileNameWithTimestamp('paper-wallet-certificate', '');
    const filePath = global.dialog.showSaveDialog({
      defaultPath: `${name}.pdf`,
      filters: [
        {
          name,
          extensions: ['pdf'],
        },
      ],
    });

    // if cancel button is clicked or path is empty
    if (!filePath) return;

    this.props.actions.wallets.generateCertificate.trigger({ filePath });
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
