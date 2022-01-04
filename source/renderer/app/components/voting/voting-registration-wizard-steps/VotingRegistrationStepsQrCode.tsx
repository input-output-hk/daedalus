import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import QRCode from 'qrcode.react';
import { set } from 'lodash';
import { observer } from 'mobx-react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';
import { NEXT_VOTING_FUND_NUMBER } from '../../../config/votingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationStepsQrCod... Remove this comment to see the full error message
import styles from './VotingRegistrationStepsQrCode.scss';

const messages = defineMessages({
  qrCodeTitle: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeTitle',
    defaultMessage: '!!!Please complete your registration now.',
    description: 'Qr code title on the voting registration "qr code" step.',
  },
  qrCodeDescription1: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeDescription1',
    defaultMessage:
      '!!!Open the Catalyst Voting app on your smartphone, scan the QR code, and enter your PIN to complete the voting registration process.',
    description:
      'Part 1 of Qr code description of use on the voting registration "qr code" step.',
  },
  qrCodeDescription2: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeDescription2',
    defaultMessage:
      '!!!Your registration remains valid across all Catalyst funding rounds. Ensure that you save your QR code and PIN so you can reconnect your wallet to the voting app if you are logged out, or if you want to connect a new device.',
    description:
      'Part 2 of Qr code description of use on the voting registration "qr code" step.',
  },
  qrCodeWarning: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeWarning',
    defaultMessage:
      '!!!<span>Warning:</span> After closing this window the QR code will no longer be available. If you do not keep a PDF copy of the QR code, you might not be able to participate in voting.',
    description: 'Qr code warning on the voting registration "qr code" step.',
  },
  checkbox1Label: {
    id: 'voting.votingRegistration.qrCode.step.checkbox1Label',
    defaultMessage:
      '!!!I understand that I will not be able to retrieve this QR code again after closing this window.',
    description:
      'First checkbox label on the voting registration "qr code" step.',
  },
  checkbox2Label: {
    id: 'voting.votingRegistration.qrCode.step.checkbox2Label',
    defaultMessage:
      '!!!I acknowledge that I must have the downloaded PDF with the QR code, to vote with Fund{nextVotingFundNumber}.',
    description:
      'Second checkbox label on the voting registration "qr code" step.',
  },
  closeButtonLabel: {
    id: 'voting.votingRegistration.qrCode.step.closeButtonLabel',
    defaultMessage: '!!!Close',
    description:
      '"Close" button label on the voting registration "qr code" step.',
  },
  saveAsPdfButtonLabel: {
    id: 'voting.votingRegistration.qrCode.step.saveAsPdfButtonLabel',
    defaultMessage: '!!!Save as PDF',
    description:
      '"Save as PDF" button label on the voting registration "qr code" step.',
  },
});
type Props = {
  onClose: (...args: Array<any>) => any;
  onDownloadPDF: (...args: Array<any>) => any;
  stepsList: Array<string>;
  activeStep: number;
  qrCode: string | null | undefined;
};
type State = {
  isCheckbox1Accepted: boolean;
  isCheckbox2Accepted: boolean;
};

@observer
class VotingRegistrationStepsQrCode extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  state = {
    isCheckbox1Accepted: false,
    isCheckbox2Accepted: false,
  };
  toggleAcceptance = (param: 'isCheckbox1Accepted' | 'isCheckbox2Accepted') =>
    this.setState((currentState) => set({}, param, !currentState[param]));
  handleClose = () => {
    const { isCheckbox1Accepted, isCheckbox2Accepted } = this.state;

    if (isCheckbox1Accepted && isCheckbox2Accepted) {
      this.props.onClose();
    }
  };

  render() {
    const { intl } = this.context;
    const { isCheckbox1Accepted, isCheckbox2Accepted } = this.state;
    const { stepsList, activeStep, qrCode, onDownloadPDF } = this.props;
    const qrCodeTitle = intl.formatMessage(messages.qrCodeTitle);
    const qrCodeDescription1 = intl.formatMessage(messages.qrCodeDescription1);
    const qrCodeDescription2 = intl.formatMessage(messages.qrCodeDescription2);
    const qrCodeWarning = <FormattedHTMLMessage {...messages.qrCodeWarning} />;
    const checkbox1Label = intl.formatMessage(messages.checkbox1Label);
    const checkbox2Label = intl.formatMessage(messages.checkbox2Label, {
      nextVotingFundNumber: NEXT_VOTING_FUND_NUMBER,
    });
    const closeButtonLabel = intl.formatMessage(messages.closeButtonLabel);
    const saveAsPdfButtonLabel = intl.formatMessage(
      messages.saveAsPdfButtonLabel
    );
    const areBothCheckboxesAccepted =
      isCheckbox1Accepted && isCheckbox2Accepted;
    // Get QRCode color value from active theme's CSS variable
    const qrCodeBackgroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-background-color'
        )
      : 'transparent';
    const qrCodeForegroundColor = document.documentElement
      ? document.documentElement.style.getPropertyValue(
          '--theme-receive-qr-code-foreground-color'
        )
      : '#000';
    const actions = [
      {
        label: closeButtonLabel,
        onClick: this.handleClose,
        disabled: !areBothCheckboxesAccepted,
      },
      {
        label: saveAsPdfButtonLabel,
        onClick: onDownloadPDF,
        primary: true,
      },
    ];
    return (
      <VotingRegistrationDialog
        onClose={this.handleClose}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        containerClassName={styles.component}
        hideCloseButton={!areBothCheckboxesAccepted}
      >
        <div className={styles.qrCode}>
          {qrCode && (
            <QRCode
              value={qrCode}
              bgColor={qrCodeBackgroundColor}
              fgColor={qrCodeForegroundColor}
              size={152}
            />
          )}
        </div>
        <div className={styles.qrCodeDescription}>
          <p className={styles.boldText}>{qrCodeTitle}</p>
          <p>{qrCodeDescription1}</p>
          <p>{qrCodeDescription2}</p>
          <p className={styles.warning}>{qrCodeWarning}</p>
        </div>
        <hr className={styles.separator} />
        <div className={styles.checkboxes}>
          <Checkbox
            label={checkbox1Label}
            onChange={() => this.toggleAcceptance('isCheckbox1Accepted')}
            className={styles.checkbox}
            checked={isCheckbox1Accepted}
          />
          <Checkbox
            label={checkbox2Label}
            onChange={() => this.toggleAcceptance('isCheckbox2Accepted')}
            className={styles.checkbox}
            checked={isCheckbox2Accepted}
          />
        </div>
      </VotingRegistrationDialog>
    );
  }
}

export default VotingRegistrationStepsQrCode;
