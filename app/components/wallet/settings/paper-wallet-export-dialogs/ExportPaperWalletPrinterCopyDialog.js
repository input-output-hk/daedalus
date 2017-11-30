// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import Checkbox from 'react-polymorph/lib/components/Checkbox';
import SimpleCheckboxSkin from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import { defineMessages, intlShape } from 'react-intl';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import styles from './ExportPaperWalletPrinterCopyDialog.scss';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.export.dialog.printerCheck.headline',
    defaultMessage: '!!!Check printer copy',
    description: 'headline for "Check printer copy" in wallet check printer copy dialog.'
  },
  continueLabel: {
    id: 'paper.wallet.export.dialog.button.continueLabel',
    defaultMessage: '!!!Continue',
    description: 'Label "Continue" on the dialog button for check printer copy in wallet check printer copy dialog.'
  },
  confirmPrinterCopyNotice: {
    id: 'paper.wallet.export.dialog.printerCheck.confirmPrinterCopyNotice',
    defaultMessage: '!!!Yes, paper wallet is successfully printed and everything is readable and scannable.',
    description: 'Notice to confirm if the paper wallet is correctly printed in wallet check printer copy dialog ',
  },
});

type Props = {
  onContinue: Function,
  onClose: Function,
  onBack: Function,
  onTogglePrinterCopyNotice: Function,
  isPrinterCopyNoticeAccepted: boolean,
};

@observer
export default class ExportPaperWalletPrinterCopyDialog extends Component<Props> {

  static defaultProps = {
    isPrinterCopyNoticeAccepted: false,
  };

  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onContinue,
      onClose,
      onBack,
      isPrinterCopyNoticeAccepted,
      onTogglePrinterCopyNotice,
    } = this.props;

    const dialogClasses = classnames([
      styles.component,
      'ExportPaperWalletPrinterCopyDialog',
    ]);

    const actions = [
      {
        label: intl.formatMessage(messages.continueLabel),
        primary: true,
        disabled: (
          !isPrinterCopyNoticeAccepted
        ),
        onClick: onContinue
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
        backButton={<DialogBackButton onBack={onBack} />}
      >
        <div className={styles.instructions}>
          <p>
            Check your paper wallet be correctly printed and everything is readable.
            You can try to scan QR codes with special app.
          </p>
        </div>

        <div className={styles.checkbox}>
          <Checkbox
            label={intl.formatMessage(messages.confirmPrinterCopyNotice)}
            onChange={onTogglePrinterCopyNotice}
            checked={isPrinterCopyNoticeAccepted}
            skin={<SimpleCheckboxSkin />}
          />
        </div>

      </Dialog>
    );
  }

}
