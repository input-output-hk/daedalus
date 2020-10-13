// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import classNames from 'classnames';
import commonStyles from './VotingAddSteps.scss';
import styles from './VotingAddStepsQrCode.scss';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import QRCode from 'qrcode.react';
import LoadingSpinner from '../../widgets/LoadingSpinner';
import ReactToolboxMobxForm from '../../../utils/ReactToolboxMobxForm';
import TinyCheckbox from '../../widgets/forms/TinyCheckbox';
import { observer } from 'mobx-react';
import globalMessages from '../../../i18n/global-messages';
import vjf from 'mobx-react-form/lib/validators/VJF';
import { FORM_VALIDATION_DEBOUNCE_WAIT } from '../../../config/timingConfig';

const messages = defineMessages({
  loadingLabel: {
    id: 'voting.votingAdd.QrCode.step.description',
    defaultMessage: '!!!Waiting for transaction to be confirmed...',
    description: 'Description on the voting add "qr code" step.',
  },
  loadingMessage: {
    id: 'voting.votingAdd.QrCode.step.loadingMessage',
    defaultMessage: '!!!Waiting for transaction to be confirmed...',
    description: 'Loading voting add message for the QR code step.',
  },
  qrCodeTitle: {
    id: 'voting.votingAdd.QrCode.step.qrCodeTitle',
    defaultMessage: '!!!Please complete your registration now.',
    description: 'Qr code title on the voting add "qr code" step.',
  },
  qrCodeDescription: {
    id: 'voting.votingAdd.QrCode.step.qrCodeDescription',
    defaultMessage: '!!!Scan the QR code using the Catalyst Voting app.',
    description: 'Qr code description of use on the voting add "qr code" step.',
  },
  readCheckboxLabel: {
    id: 'voting.votingAdd.QrCode.step.readCheckboxLabel',
    defaultMessage:
      '!!!I undestand that this is last time I see this QR code before closing. Next time I will need to generate new one and pay feed again',
    description: 'Label for read checkbox on the voting add "qr code" step..',
  },
  continueButtonLabel: {
    id: 'voting.votingAdd.QrCode.step.continueButtonLabel',
    defaultMessage: '!!!Close',
    description: 'Label for continue button on the voting add "qr code" step.',
  },
});

messages.fieldIsRequired = globalMessages.fieldIsRequired;

type Props = {
  onClose: Function,
  qrCode: string,
  isSubmitting: Boolean,
};

@observer
export default class VotingAddStepsQrCode extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  form = new ReactToolboxMobxForm(
    {
      fields: {
        readChecked: {
          type: 'checkbox',
          label: this.context.intl.formatMessage(messages.readCheckboxLabel),
          validators: [
            ({ field }) => {
              const checkbox = field.value;
              if (!checkbox) {
                return [
                  false,
                  this.context.intl.formatMessage(messages.fieldIsRequired),
                ];
              }
              return [true];
            },
          ],
        },
      },
    },
    {
      plugins: { vjf: vjf() },
      options: {
        validateOnChange: true,
        validationDebounceWait: FORM_VALIDATION_DEBOUNCE_WAIT,
      },
    }
  );

  render() {
    const { form } = this;
    const { intl } = this.context;
    const { onClose, qrCode, isSubmitting } = this.props;
    const readCheckboxField = form.$('readChecked');

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const loadingMessage = intl.formatMessage(messages.loadingMessage);
    const qrCodeTitle = intl.formatMessage(messages.qrCodeTitle);
    const qrCodeDescription = intl.formatMessage(messages.qrCodeDescription);

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
          {isSubmitting || !qrCode ? (
            <div className={styles.loadinBlockWrapper}>
              <LoadingSpinner />
              <p>{loadingMessage}</p>
            </div>
          ) : (
            <>
              <div className={styles.qrCode}>
                <QRCode
                  value={qrCode}
                  bgColor={qrCodeBackgroundColor}
                  fgColor={qrCodeForegroundColor}
                  size={152}
                />
              </div>
              <div className={styles.qrCodeDescription}>
                <p>{qrCodeTitle}</p>
                <p>{qrCodeDescription}</p>
              </div>
              <hr className={styles.separator} />
              <div className={styles.qrCodeConfirm}>
                <TinyCheckbox {...readCheckboxField.bind()} />
              </div>
            </>
          )}
        </div>
        <Button
          onClick={onClose}
          skin={ButtonSkin}
          label={buttonLabel}
          disabled={!readCheckboxField.isValid || isSubmitting || !qrCode}
        />
      </div>
    );
  }
}
