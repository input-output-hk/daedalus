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
    const locale = this.props.stores.profile.currentLocale;

    moment.updateLocale(locale, {
      longDateFormat: {
        pdfTimestamp: intl.formatMessage(messages.timestamp),
      },
    });
    const localizedDateFormat = moment()
      .localeData()
      .longDateFormat('pdfTimestamp');
    const timestamp = moment(date).format(localizedDateFormat);
    const name = generateFileNameWithTimestamp({
      prefix: 'paper-wallet-certificate',
      date,
      fileType: '',
      isUTC: false,
    });

    try {
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
    } catch (error) {
      console.log('ERROR: -----------');
      console.log(error);

      alert(
        'Please, close the existing PDF file or save with a different name'
      );

      return false;
    }
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
