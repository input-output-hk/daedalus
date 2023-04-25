import React, { Component, Fragment } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/untada.... Remove this comment to see the full error message
import sadLogo from '../../../assets/images/untada.inline.svg';
import ProgressBarLarge from '../../widgets/ProgressBarLarge';
import { VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS } from '../../../config/votingConfig';
import LocalizableError from '../../../i18n/LocalizableError';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationStepsConfi... Remove this comment to see the full error message
import styles from './VotingRegistrationStepsConfirm.scss';
import VotingRegistrationDialog from './widgets/VotingRegistrationDialog';

const messages = defineMessages({
  description: {
    id: 'voting.votingRegistration.confirm.step.description',
    defaultMessage:
      '!!!Confirmation of voting registration requires approximately 5 minutes. Please leave Daedalus running.',
    description: 'Description voting registration "confirm" step.',
  },
  descriptionRestart: {
    id: 'voting.votingRegistration.confirm.step.descriptionRestart',
    defaultMessage:
      '!!!Please restart the voting registration process by clicking <span>Restart voting registration</span>.',
    description:
      'Message for restart voting registration on the voting registration "confirm" step.',
  },
  errorMessage: {
    id: 'voting.votingRegistration.confirm.step.errorMessage',
    defaultMessage:
      '!!!The voting registration process was not completed correctly.',
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
    defaultMessage: '!!!Restart voting registration',
    description:
      'Label for restart button on the voting registration "confirm" step.',
  },
  transactionPendingLabel: {
    id: 'voting.votingRegistration.confirm.step.transactionPendingLabel',
    defaultMessage: '!!!Transaction pending...',
    description:
      'Label for pending transaction state on the voting registration "confirm" step.',
  },
  transactionConfirmedLabel: {
    id: 'voting.votingRegistration.confirm.step.transactionConfirmedLabel',
    defaultMessage: '!!!Transaction confirmed',
    description:
      'Label for confirmed transaction state on the voting registration "confirm" step.',
  },
  waitingForConfirmationsLabel: {
    id: 'voting.votingRegistration.confirm.step.waitingForConfirmationsLabel',
    defaultMessage: '!!!Waiting for confirmation...',
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
  onClose: (...args: Array<any>) => any;
  stepsList: Array<string>;
  activeStep: number;
  isTransactionPending: boolean;
  isTransactionConfirmed: boolean;
  transactionConfirmations: number;
  transactionError:
    | (boolean | null | undefined)
    | (LocalizableError | null | undefined);
  onConfirm: (...args: Array<any>) => any;
  onRestart: (...args: Array<any>) => any;
};

@observer
class VotingRegistrationStepsConfirm extends Component<Props> {
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
    let progressBarLeftLabelMessage;
    if (isTransactionConfirmed)
      progressBarLeftLabelMessage = messages.transactionConfirmedLabel;
    else if (isTransactionPending)
      progressBarLeftLabelMessage = messages.transactionPendingLabel;
    else progressBarLeftLabelMessage = messages.waitingForConfirmationsLabel;
    const progressBarLeftLabel = intl.formatMessage(
      progressBarLeftLabelMessage
    );
    const progressBarRightLabel = isTransactionPending
      ? ''
      : intl.formatMessage(messages.confirmationsCountLabel, {
          currentCount: Math.min(
            transactionConfirmations,
            VOTING_REGISTRATION_MIN_TRANSACTION_CONFIRMATIONS
          ),
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

export default VotingRegistrationStepsConfirm;
