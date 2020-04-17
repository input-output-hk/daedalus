// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import moment from 'moment';
import { showSaveDialogChannel } from '../../../../ipc/show-file-dialog-channels';
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

  onPrint = async () => {
    const {
      currentDateFormat,
      currentTimeFormatShort,
    } = this.props.stores.profile;
    const date = moment();
    const formattedDate = date.format(currentDateFormat);
    const formattedTime = date.format(currentTimeFormatShort);
    const timestamp = `${formattedDate} - ${formattedTime}`;

    const name = generateFileNameWithTimestamp({
      prefix: 'paper-wallet-certificate',
      date,
      extension: '',
      isUTC: false,
    });
    const params = {
      defaultPath: `${name}.pdf`,
      filters: [
        {
          name,
          extensions: ['pdf'],
        },
      ],
    };

    const { filePath } = await showSaveDialogChannel.send(params);

    // if cancel button is clicked or path is empty
    if (!filePath) return;

    this.props.actions.wallets.generateCertificate.trigger({
      filePath,
      timestamp,
    });
  };

  handleOpenExternalLink = (url: string) => {
    const { openExternalLink } = this.props.stores.app;
    openExternalLink(url);
  };

  render() {
    const { wallets, app } = this.props.stores;
    const {
      environment: { network, rawNetwork },
    } = app;
    return (
      <InstructionsDialog
        inProgress={wallets.generatingCertificateInProgress}
        error={wallets.generatingCertificateError}
        network={network}
        rawNetwork={rawNetwork}
        onPrint={this.onPrint}
        onClose={this.props.onClose}
        onOpenExternalLink={this.handleOpenExternalLink}
      />
    );
  }
}
