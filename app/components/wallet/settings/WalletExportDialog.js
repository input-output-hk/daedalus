// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import Dialog from '../../widgets/Dialog';
import WalletExportDialogChoices from './WalletExportDialogChoices';
import styles from './WalletExportDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'wallet.export.dialog.headline',
    defaultMessage: '!!!Export',
    description: 'headline for "export paper wallet" dialog.'
  },
  printLabel: {
    id: 'paper.wallet.export.dialog.button.printLabel',
    defaultMessage: '!!!Print',
    description: 'Label "Print" on the dialog button for export paper wallet dialog.'
  },
});

@observer
export default class WalletExportDialog extends Component {

  props: {
    onPrint: Function,
    onClose: Function,
    onChooseWalletExportType: Function,
    walletExportType: string,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { onPrint, onClose, walletExportType, onChooseWalletExportType } = this.props;
    const dialogClasses = classnames([
      styles.component,
      'WalletExportDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.printLabel),
        primary: true,
        onClick: onPrint,
      }
    ];

    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        closeOnOverlayClick
        onClose={onClose}
        closeButton={<DialogCloseButton onClose={onClose} />}
      >
        <WalletExportDialogChoices
          activeChoice={walletExportType}
          onSelectChoice={(choice: string) => {
            onChooseWalletExportType(choice);
          }}
        />

        <div className={styles.instructions}>
          <p>Instructions:</p>
          <ul>
            <li>
                Because of creating new address to store your money,
                new wallet will be created.
            </li>
            <li>All funds will be transfered there. </li>
            <li>Paper certificate will be printed.</li>
            <li>Mnemonic phrase should be written down on your certificate.</li>
            <li>Store paper wallet safe.</li>
          </ul>
        </div>

      </Dialog>
    );
  }

}
