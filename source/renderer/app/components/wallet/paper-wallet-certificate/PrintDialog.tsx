import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import { defineMessages, intlShape } from 'react-intl';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import { CheckboxSkin } from 'react-polymorph/lib/skins/simple/CheckboxSkin';
import Dialog from '../../widgets/Dialog';
import DialogCloseButton from '../../widgets/DialogCloseButton';
import globalMessages from '../../../i18n/global-messages';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PrintDialog.scss' or its cor... Remove this comment to see the full error message
import styles from './PrintDialog.scss';
import {
  PAPER_WALLET_PRINTED_WORDS_COUNT,
  PAPER_WALLET_WRITTEN_WORDS_COUNT,
} from '../../../config/cryptoConfig';

const messages = defineMessages({
  headline: {
    id: 'paper.wallet.create.certificate.print.dialog.headline',
    defaultMessage: '!!!Verify printed certificate',
    description:
      'Headline for the "Paper wallet create certificate print dialog".',
  },
  subtitle: {
    id: 'paper.wallet.create.certificate.print.dialog.subtitle',
    defaultMessage: `!!!Check your paper wallet certificate and make sure everything
      is readable and correctly printed. You can test this by scanning the QR code with
      a QR scanner application on your mobile phone.`,
    description: '"Paper wallet create certificate print dialog" subtitle.',
  },
  info: {
    id: 'paper.wallet.create.certificate.print.dialog.info',
    defaultMessage: `!!!Your certificate is not yet complete and does not contain all
      the data needed to restore your paper wallet. In the next step, you will need to
      write down an additional {paperWalletWrittenWordsCount} words to your paper wallet recovery phrase.`,
    description: '"Paper wallet create certificate print dialog" info.',
  },
  certificatePrintedConfirmationLabel: {
    id:
      'paper.wallet.create.certificate.print.dialog.certificatePrintedConfirmation',
    defaultMessage:
      '!!!Yes, the paper wallet certificate printed successfully.',
    description:
      '"Paper wallet create certificate print dialog" certificate printed confirmation.',
  },
  certificateReadableConfirmationLabel: {
    id:
      'paper.wallet.create.certificate.print.dialog.certificateReadableConfirmation',
    defaultMessage:
      '!!!Yes, first {paperWalletPrintedWordsCount} words of the paper wallet recovery phrase are readable.',
    description:
      '"Paper wallet create certificate print dialog" certificate readable confirmation.',
  },
  qrScannableConfirmationLabel: {
    id: 'paper.wallet.create.certificate.print.dialog.qrScannableConfirmation',
    defaultMessage: '!!!Yes, the QR code is scannable.',
    description:
      '"Paper wallet create certificate print dialog" QR scannable confirmation.',
  },
});
type State = {
  isPrintedCorrectly: boolean;
  isReadable: boolean;
  isScannable: boolean;
};
type Props = {
  onContinue: (...args: Array<any>) => any;
  onClose: (...args: Array<any>) => any;
};

@observer
class PrintDialog extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isPrintedCorrectly: false,
    isReadable: false,
    isScannable: false,
  };
  onConfirmCorrectPrinting = () => {
    this.setState((prevState) => ({
      isPrintedCorrectly: !prevState.isPrintedCorrectly,
    }));
  };
  onConfirmReadable = () => {
    this.setState((prevState) => ({
      isReadable: !prevState.isReadable,
    }));
  };
  onConfirmScannable = () => {
    this.setState((prevState) => ({
      isScannable: !prevState.isScannable,
    }));
  };

  render() {
    const { intl } = this.context;
    const { onContinue, onClose } = this.props;
    const { isPrintedCorrectly, isReadable, isScannable } = this.state;
    const dialogClasses = classnames([styles.component, 'printDialog']);
    const certificatePrintedCheckboxClasses = classnames([
      'printedCheckbox',
      styles.checkbox,
    ]);
    const certificateReadableCheckboxClasses = classnames([
      'readableCheckbox',
      styles.checkbox,
    ]);
    const qrScannableCheckboxClasses = classnames([
      'scannableCheckbox',
      styles.checkbox,
    ]);
    const canSubmit = isPrintedCorrectly && isReadable && isScannable;
    const actions = [
      {
        className: 'continueButton',
        label: intl.formatMessage(globalMessages.dialogButtonContinueLabel),
        primary: true,
        disabled: !canSubmit,
        onClick: onContinue,
      },
    ];
    return (
      <Dialog
        className={dialogClasses}
        title={intl.formatMessage(messages.headline)}
        actions={actions}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
      >
        <div className={styles.printContentWrapper}>
          <p className={styles.subtitle}>
            {intl.formatMessage(messages.subtitle)}
          </p>
          <p className={styles.info}>
            {intl.formatMessage(messages.info, {
              paperWalletWrittenWordsCount: PAPER_WALLET_WRITTEN_WORDS_COUNT,
            })}
          </p>
          <div className={styles.content}>
            <Checkbox
              className={certificatePrintedCheckboxClasses}
              label={intl.formatMessage(
                messages.certificatePrintedConfirmationLabel
              )}
              onChange={this.onConfirmCorrectPrinting}
              checked={isPrintedCorrectly}
              skin={CheckboxSkin}
            />

            <Checkbox
              className={certificateReadableCheckboxClasses}
              label={intl.formatMessage(
                messages.certificateReadableConfirmationLabel,
                {
                  paperWalletPrintedWordsCount: PAPER_WALLET_PRINTED_WORDS_COUNT,
                }
              )}
              onChange={this.onConfirmReadable}
              checked={isReadable}
              skin={CheckboxSkin}
            />

            <Checkbox
              className={qrScannableCheckboxClasses}
              label={intl.formatMessage(messages.qrScannableConfirmationLabel)}
              onChange={this.onConfirmScannable}
              checked={isScannable}
              skin={CheckboxSkin}
            />
          </div>
        </div>
      </Dialog>
    );
  }
}

export default PrintDialog;
