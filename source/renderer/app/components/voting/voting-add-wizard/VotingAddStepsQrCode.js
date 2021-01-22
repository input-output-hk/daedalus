// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import QRCode from 'qrcode.react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import openAppIcon from '../../../assets/images/voting/open-app-ic.inline.svg';
import commonStyles from './VotingAddSteps.scss';
import styles from './VotingAddStepsQrCode.scss';

const messages = defineMessages({
  qrCodeTitle: {
    id: 'voting.votingAdd.qrCode.step.qrCodeTitle',
    defaultMessage: '!!!Please complete your registration now.',
    description: 'Qr code title on the voting add "qr code" step.',
  },
  qrCodeDescription: {
    id: 'voting.votingAdd.qrCode.step.qrCodeDescription',
    defaultMessage: '!!!Scan the QR code using the Catalyst Voting app.',
    description: 'Qr code description of use on the voting add "qr code" step.',
  },
  message: {
    id: 'voting.votingAdd.qrCode.step.message',
    defaultMessage:
      '!!!This QR code is not saved anywhere, and you will not have access to it after closing this screen.',
    description: 'Qr code messakes of use on the voting add "qr code" step.',
  },
  takeScreenShotMessage: {
    id: 'voting.votingAdd.qrCode.step.takeScreenShotMessage',
    defaultMessage:
      '!!!Take a screenshot of the QR code if you want to access it later.',
    description:
      'Qr code second message for take a screen shot used on the voting add "qr code" step.',
  },
  warningMessage: {
    id: 'voting.votingAdd.qrCode.step.warningMessage',
    defaultMessage:
      '!!!If you lose access to the QR code, you will need to register again in order to generate a new QR code.',
    description:
      'Qr code warning messages of use on the voting add "qr code" step.',
  },
});

type Props = {
  qrCode: string | null,
};

@observer
export default class VotingAddStepsQrCode extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const { qrCode } = this.props;

    const qrCodeTitle = intl.formatMessage(messages.qrCodeTitle);
    const qrCodeDescription = intl.formatMessage(messages.qrCodeDescription);
    const message = intl.formatMessage(messages.message);
    const takeScreenShotMessage = intl.formatMessage(
      messages.takeScreenShotMessage
    );
    const warningMessage = intl.formatMessage(messages.warningMessage);

    const className = classNames([
      commonStyles.votingAddSteps,
      styles.votingAddStepsQrCodeWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

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

    return (
      <div className={className}>
        <div className={contentClassName}>
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
            <SVGInline svg={openAppIcon} className={styles.qrCodeMessageIcon} />
            <div className={styles.qrCodeMessageText}>
              <p>{qrCodeTitle}</p>
              <p>{qrCodeDescription}</p>
            </div>
          </div>
          <hr className={styles.separator} />
          <div className={styles.messages}>
            <p>{message}</p>
            <p className={styles.secondaryWarning}>{takeScreenShotMessage}</p>
            <p className={styles.primaryWarning}>{warningMessage}</p>
          </div>
        </div>
      </div>
    );
  }
}
