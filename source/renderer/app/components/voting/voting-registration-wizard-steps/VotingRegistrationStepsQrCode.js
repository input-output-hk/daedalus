// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import QRCode from 'qrcode.react';
import { observer } from 'mobx-react';
import { Checkbox } from 'react-polymorph/lib/components/Checkbox';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';
import styles from './VotingRegistrationStepsQrCode.scss';

const messages = defineMessages({
  qrCodeTitle: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeTitle',
    defaultMessage: '!!!Please complete your registration now.',
    description: 'Qr code title on the voting registration "qr code" step.',
  },
  qrCodeDescription: {
    id: 'voting.votingRegistration.qrCode.step.qrCodeDescription',
    defaultMessage:
      '!!!Open the Catalyst Voting app on your smartphone, scan the QR code and use the PIN to complete the voting registration.',
    description:
      'Qr code description of use on the voting registration "qr code" step.',
  },
  checkbox1Label: {
    id: 'voting.votingRegistration.qrCode.step.checkbox1Label',
    defaultMessage:
      '!!!I understand that I will not be able to get this QR code again after closing this window.',
    description:
      'First checkbox label on the voting registration "qr code" step.',
  },
  checkbox2Label: {
    id: 'voting.votingRegistration.qrCode.step.checkbox2Label',
    defaultMessage:
      '!!!I acknowledge that I need to have the downloaded PDF with the QR code to be able to vote with Fund3 ',
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
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  qrCode: ?string,
};

@observer
export default class VotingRegistrationStepsQrCode extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  handleClose = () => {
    this.props.onClose(true);
  };

  render() {
    const { intl } = this.context;
    const { stepsList, activeStep, qrCode } = this.props;

    const qrCodeTitle = intl.formatMessage(messages.qrCodeTitle);
    const qrCodeDescription = intl.formatMessage(messages.qrCodeDescription);
    const checkbox1Label = intl.formatMessage(messages.checkbox1Label);
    const checkbox2Label = intl.formatMessage(messages.checkbox2Label);
    const closeButtonLabel = intl.formatMessage(messages.closeButtonLabel);
    const saveAsPdfButtonLabel = intl.formatMessage(
      messages.saveAsPdfButtonLabel
    );

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
        disabled: true,
      },
      {
        label: saveAsPdfButtonLabel,
        onClick: () => {},
        primary: true,
      },
    ];

    return (
      <VotingRegistrationDialog
        onClose={this.handleClose}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        closeOnOverlayClick
        containerClassName={styles.component}
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
          <p>{qrCodeDescription}</p>
        </div>
        <hr className={styles.separator} />
        <div className={styles.checkboxes}>
          <Checkbox
            label={checkbox1Label}
            onChange={() => {}}
            className={styles.checkbox}
            checked={false}
          />
          <Checkbox
            label={checkbox2Label}
            onChange={() => {}}
            className={styles.checkbox}
            checked={false}
            disabled
          />
        </div>
      </VotingRegistrationDialog>
    );
  }
}
