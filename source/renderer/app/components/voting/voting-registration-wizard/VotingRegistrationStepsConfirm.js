// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Button } from 'react-polymorph/lib/components/Button';
import { ButtonSkin } from 'react-polymorph/lib/skins/simple/ButtonSkin';
import { observer } from 'mobx-react';
import classNames from 'classnames';
import SVGInline from 'react-svg-inline';
import confirmMessageIcon from '../../../assets/images/voting/confirm-step-message-ic.inline.svg';
import confirmErrorMessageIcon from '../../../assets/images/voting/confirm-step-error-message-ic.inline.svg';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './VotingRegistrationStepsConfirm.scss';
import commonStyles from './VotingRegistrationSteps.scss';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.confirm.step.description',
    defaultMessage:
      '!!!To ensure your registration is confirmed, you will need to wait 1 hour before proceeding.',
    description: 'Description voting registration "confirm" step.',
  },
  descriptionRestart: {
    id: 'voting.votingRegistration.confirm.step.descriptionRestart',
    defaultMessage:
      '!!!Please restart the registration process by clicking the Restart Registration button.',
    description:
      'Message for restart voting registration on the voting registration "confirm" step.',
  },
  importantInformation1: {
    id: 'voting.votingRegistration.confirm.step.importantInformation1',
    defaultMessage:
      '!!!If you close this window, or the wallet, you will need to start the registration process again and resubmit your registration transaction.',
    description:
      'First messages for alert to users on the voting registration "confirm" step.',
  },
  importantInformation2: {
    id: 'voting.votingRegistration.confirm.step.importantInformation2',
    defaultMessage:
      '!!!In 99.99% of cases the 1 hour wait time should suffice, however if you want a 100% guarantee to ensure successful registration we recommend waiting for 18 hours before confirming.',
    description:
      'Second messages for alert the user on the voting registration "confirm" step.',
  },
  errorMessage: {
    id: 'voting.votingRegistration.confirm.step.errorMessage',
    defaultMessage: '!!!Registration transaction was not registered correctly.',
    description: 'Error message on the voting registration "confirm" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.continueButtonLabel',
    defaultMessage: '!!!Confirm registration Transaction',
    description:
      'Label for continue button on the voting registration "confirm" step.',
  },
  continueButtonCountDownLabel: {
    id: 'voting.votingRegistration.confirm.step.continueButtonCountDownLabel',
    defaultMessage:
      '!!![{countdownRemaining} seconds left before confirmation]',
    description:
      'Label for continue button countdown on the voting registration "confirm" step.',
  },
  restartButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.restartButtonLabel',
    defaultMessage: '!!!Restart Registration',
    description:
      'Label for restart button on the voting registration "confirm" step.',
  },
});

type Props = {
  onConfirm: Function,
  onRollback: Function,
  isSubmitting: boolean,
  isTransactionApproved: boolean,
  transactionError: ?LocalizableError,
  countdownRemaining: number,
};

@observer
export default class VotingRegistrationStepsConfirm extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onConfirm,
      onRollback,
      isSubmitting,
      isTransactionApproved,
      transactionError,
      countdownRemaining,
    } = this.props;

    const description = intl.formatMessage(messages.description);
    const importantInformation1 = intl.formatMessage(
      messages.importantInformation1
    );
    const importantInformation2 = intl.formatMessage(
      messages.importantInformation2
    );
    const descriptionRestart = intl.formatMessage(messages.descriptionRestart);
    const errorMessage = intl.formatMessage(messages.errorMessage);
    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const buttonCountDownLabel = intl.formatMessage(
      messages.continueButtonCountDownLabel,
      { countdownRemaining }
    );
    const restartButtonLabel = intl.formatMessage(messages.restartButtonLabel);

    const className = classNames([
      commonStyles.votingRegistrationSteps,
      styles.votingRegistrationStepsConfirmWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

    const countdownDisplay =
      countdownRemaining > 0
        ? `${buttonLabel} ${buttonCountDownLabel}`
        : buttonLabel;

    return (
      <div className={className}>
        <div className={contentClassName}>
          {!transactionError &&
          (isTransactionApproved || countdownRemaining > 0 || isSubmitting) ? (
            <>
              <div className={styles.header}>
                <SVGInline
                  svg={confirmMessageIcon}
                  className={styles.descriptionIcon}
                />
              </div>
              <div className={styles.description}>
                <p>{description}</p>
              </div>
              <div className={styles.messages}>
                <p>{importantInformation1}</p>
                <p>{importantInformation2}</p>
              </div>
              <hr className={styles.separator} />
              <div className={styles.buttonContainer}>
                <Button
                  className={styles.buttonConfirmStyles}
                  onClick={onConfirm}
                  skin={ButtonSkin}
                  label={countdownDisplay}
                  disabled={isSubmitting || countdownRemaining > 0}
                />
              </div>
            </>
          ) : (
            <>
              <div className={styles.header}>
                <SVGInline
                  svg={confirmErrorMessageIcon}
                  className={styles.errorIcon}
                />
              </div>
              <div className={styles.errorMessage}>
                <p>
                  {transactionError
                    ? intl.formatMessage(transactionError)
                    : errorMessage}
                </p>
              </div>
              <div className={styles.description}>
                <p>{descriptionRestart}</p>
              </div>
              <div className={styles.buttonContainer}>
                <Button
                  onClick={onRollback}
                  skin={ButtonSkin}
                  label={restartButtonLabel}
                />
              </div>
            </>
          )}
        </div>
      </div>
    );
  }
}
