// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import { defineMessages, intlShape } from 'react-intl';
import moment from 'moment';
import InstructionsDialog from '../../../../components/wallet/paper-wallet-certificate/InstructionsDialog';
import type { InjectedDialogContainerProps } from '../../../../types/injectedPropsType';
import { generateFileNameWithTimestamp } from '../../../../../../common/utils/files';

const messages = defineMessages({
  timestamp: {
    id: 'paper.wallet.create.certificate.instructions.dialog.timestamp',
    defaultMessage: '!!!MMMM D, YYYY - h:mm A',
    description: 'Timestamp for the Paper Wallet PDF content.',
  },
});

type Props = InjectedDialogContainerProps;

@inject('stores', 'actions')
@observer
export default class InstructionsDialogContainer extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    actions: null,
    stores: null,
    children: null,
    onClose: () => {},
  };

  onPrint = () => {
    const { intl } = this.context;
    const date = moment();
    const localizedTimestampFormat = intl.formatMessage(messages.timestamp);
    const timestamp = moment(date).format(localizedTimestampFormat);

    const name = generateFileNameWithTimestamp({
      prefix: 'paper-wallet-certificate',
      date,
      extension: '',
      isUTC: false,
    });

    // TODO: refactor this direct access to the dialog api
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

    this.props.actions.wallets.generateCertificate.trigger({
      filePath,
      timestamp,
    });
  };

  render() {
    const { wallets, app } = this.props.stores;
    const {
      openExternalLink,
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
        onOpenExternalLink={openExternalLink}
      />
    );
  }
}
