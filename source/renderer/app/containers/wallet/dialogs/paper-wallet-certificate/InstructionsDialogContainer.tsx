import React, { Component } from 'react';
import path from 'path';
import { observer, inject } from 'mobx-react';
import moment from 'moment';
import { showSaveDialogChannel } from '../../../../ipc/show-file-dialog-channels';
import InstructionsDialog from '../../../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../../common/utils/files';
import type { FileDialogRequestParams } from '../../../../../../common/types/file-dialog.types';

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
class InstructionsDialogContainer extends Component<Props> {
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
    const { desktopDirectoryPath } = this.props.stores.profile;
    const defaultPath: string = path.join(desktopDirectoryPath, `${name}.pdf`);
    const params: FileDialogRequestParams = {
      defaultPath,
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
      environment: { network },
    } = app;
    return (
      <InstructionsDialog
        inProgress={wallets.generatingCertificateInProgress}
        error={wallets.generatingCertificateError}
        network={network}
        onPrint={this.onPrint}
        onClose={this.props.onClose}
        onOpenExternalLink={this.handleOpenExternalLink}
      />
    );
  }
}

export default InstructionsDialogContainer;
