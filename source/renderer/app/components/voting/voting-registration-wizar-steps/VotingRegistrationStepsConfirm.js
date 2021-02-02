// @flow
import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import confirmErrorMessageIcon from '../../../assets/images/voting/confirm-step-error-message-ic.inline.svg';
import ProgressBarLarge from '../../widgets/ProgressBarLarge';
import { VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS } from '../../../config/votingConfig';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './VotingRegistrationStepsConfirm.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

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
  message1: {
    id: 'voting.votingRegistration.confirm.step.message1',
    defaultMessage:
      '!!!Approximately 5 minutes of waiting is required for your voting registration to be confirmed. Please leave Daedalus running.',
    description:
      'message1 for alert to users on the voting registration "confirm" step.',
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
  restartButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.restartButtonLabel',
    defaultMessage: '!!!Restart Registration',
    description:
      'Label for restart button on the voting registration "confirm" step.',
  },
});

type Props = {
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  isTransactionPending: boolean,
  isTransactionConfirmed: boolean,
  transactionConfirmations: number,
  transactionError: ?boolean | ?LocalizableError,
  onConfirm: Function,
  onRestart: Function,
};

@observer
export default class VotingRegistrationStepsConfirm extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onClose,
      stepsList,
      activeStep,
      onConfirm,
      onRestart,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      transactionError,
    } = this.props;

    const message1 = intl.formatMessage(messages.message1);
    const descriptionRestart = intl.formatMessage(messages.descriptionRestart);
    const errorMessage = intl.formatMessage(messages.errorMessage);

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const restartButtonLabel = intl.formatMessage(messages.restartButtonLabel);

    const actions = [
      transactionError
        ? {
            label: restartButtonLabel,
            onClick: onRestart,
          }
        : {
            label: buttonLabel,
            onClick: onConfirm,
            disabled: !isTransactionConfirmed,
            className: styles.buttonConfirmStyles,
            primary: true,
          },
    ];

    const progressBarLeftLabel = isTransactionPending
      ? 'Transaction pending...'
      : 'Waiting for confirmations...';

    const progressBarRightLabel = isTransactionPending
      ? ''
      : `${transactionConfirmations} of ${VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS}`;

    const progress =
      (transactionConfirmations * 100) /
      VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS;

    return (
      <VotingRegistrationDialog
        onClose={onClose}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        containerClassName={styles.component}
      >
        {transactionError ? (
          <Fragment>
            <div className={styles.header}>
              <SVGInline
                svg={confirmErrorMessageIcon}
                className={styles.errorIcon}
              />
            </div>
            <div className={styles.errorMessage}>
              <p>{errorMessage}</p>
            </div>
            <div className={styles.description}>
              <p>{descriptionRestart}</p>
            </div>
          </Fragment>
        ) : (
          <Fragment>
            <ProgressBarLarge
              leftLabel={progressBarLeftLabel}
              rightLabel1={progressBarRightLabel}
              loading={isTransactionPending}
              progress={progress}
            />
            <div className={styles.messages}>
              <p>{message1}</p>
            </div>
          </Fragment>
        )}
      </VotingRegistrationDialog>
    );
  }
}
