// @flow
import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import sadLogo from '../../../assets/images/untada.inline.svg';
import ProgressBarLarge from '../../widgets/ProgressBarLarge';
import { VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS } from '../../../config/votingConfig';
import LocalizableError from '../../../i18n/LocalizableError';
import styles from './VotingRegistrationStepsConfirm.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.confirm.step.description',
    defaultMessage:
      '!!!Approximately 5 minutes of waiting is required for your voting registration to be confirmed. Please leave Daedalus running.',
    description: 'Description voting registration "confirm" step.',
  },
  descriptionRestart: {
    id: 'voting.votingRegistration.confirm.step.descriptionRestart',
    defaultMessage:
      '!!!Please restart the registration process by clicking the <span>Restart registration</span> button.',
    description:
      'Message for restart voting registration on the voting registration "confirm" step.',
  },
  errorMessage: {
    id: 'voting.votingRegistration.confirm.step.errorMessage',
    defaultMessage: '!!!Registration transaction was not registered correctly.',
    description: 'Error message on the voting registration "confirm" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.continueButtonLabel',
    defaultMessage: '!!!Continue',
    description:
      'Label for continue button on the voting registration "confirm" step.',
  },
  restartButtonLabel: {
    id: 'voting.votingRegistration.confirm.step.restartButtonLabel',
    defaultMessage: '!!!Restart registration',
    description:
      'Label for restart button on the voting registration "confirm" step.',
  },
  transactionPendingLabel: {
    id: 'voting.votingRegistration.confirm.step.transactionPendingLabel',
    defaultMessage: '!!!Transaction pending...',
    description:
      'Label for pending transaction state on the voting registration "confirm" step.',
  },
  waitingForConfirmationsLabel: {
    id: 'voting.votingRegistration.confirm.step.waitingForConfirmationsLabel',
    defaultMessage: '!!!Waiting for confirmations...',
    description:
      'Label for confirming transaction state on the voting registration "confirm" step.',
  },
  confirmationsCountLabel: {
    id: 'voting.votingRegistration.confirm.step.confirmationsCountLabel',
    defaultMessage: '!!!{currentCount} of {expectedCount}',
    description:
      'Label for number of confirmations on the voting registration "confirm" step.',
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
      stepsList,
      activeStep,
      onConfirm,
      onRestart,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      transactionError,
      onClose,
    } = this.props;

    const description = intl.formatMessage(messages.description);
    const descriptionRestart = (
      <FormattedHTMLMessage {...messages.descriptionRestart} />
    );
    const errorMessage = intl.formatMessage(messages.errorMessage);

    const buttonLabel = intl.formatMessage(messages.continueButtonLabel);
    const restartButtonLabel = intl.formatMessage(messages.restartButtonLabel);

    const actions = [
      transactionError
        ? {
            className: 'attention',
            label: restartButtonLabel,
            onClick: onRestart,
          }
        : {
            className: styles.buttonConfirmStyles,
            label: buttonLabel,
            onClick: onConfirm,
            disabled: !isTransactionConfirmed,
            primary: true,
          },
    ];

    const progressBarLeftLabel = isTransactionPending
      ? intl.formatMessage(messages.transactionPendingLabel)
      : intl.formatMessage(messages.waitingForConfirmationsLabel);

    const progressBarRightLabel = isTransactionPending
      ? ''
      : intl.formatMessage(messages.confirmationsCountLabel, {
          currentCount: transactionConfirmations,
          expectedCount: VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS,
        });

    const progress =
      (transactionConfirmations * 100) /
      VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS;

    return (
      <VotingRegistrationDialog
        onClose={() => {
          onClose(!transactionError);
        }}
        stepsList={stepsList}
        activeStep={activeStep}
        actions={actions}
        closeOnOverlayClick
        containerClassName={styles.component}
        hideSteps={!!transactionError}
      >
        {transactionError ? (
          <Fragment>
            <div className={styles.sadLogoContainer}>
              <SVGInline svg={sadLogo} className={styles.sadLogoIcon} />
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
              progress={Math.min(progress, 100)}
            />
            <div className={styles.description}>
              <p>{description}</p>
            </div>
          </Fragment>
        )}
      </VotingRegistrationDialog>
    );
  }
}
