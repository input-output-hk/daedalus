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
import styles from './VotingAddStepsConfirm.scss';
import commonStyles from './VotingAddSteps.scss';

const messages = defineMessages({
  description: {
    id: 'voting.votingAdd.confirm.step.description',
    defaultMessage:
      '!!!To ensure your registration is confirmed, you are required to wait 1 hour before proceeding.',
    description: 'Description voting add "confirm" step.',
  },
  descriptionRestart: {
    id: 'voting.votingAdd.confirm.step.descriptionRestart',
    defaultMessage:
      '!!!Please restart the registration process by clicking on the restart registration button..',
    description:
      'Message for restart voting registration on the voting add "confirm" step.',
  },
  importantInformation1: {
    id: 'voting.votingAdd.confirm.step.importantInformation1',
    defaultMessage:
      '!!!If you close this window or the wallet, you will need to start registration again and resubmit your registration transaction.',
    description:
      'First messages for alert to users on the voting add "confirm" step.',
  },
  importantInformation2: {
    id: 'voting.votingAdd.confirm.step.importantInformation2',
    defaultMessage:
      '!!!While the 1 hour wait time would suffice for 99.99% of cases, to insure 100% your registration is successful please wait a total of 18 hours before proceeding.',
    description:
      'Second messages for alert the user on the voting add "confirm" step.',
  },
  errorMessage: {
    id: 'voting.votingAdd.confirm.step.errorMessage',
    defaultMessage: '!!!Registration transaction was not registered correctly.',
    description: 'Error message on the voting add "confirm" step.',
  },
  continueButtonLabel: {
    id: 'voting.votingAdd.confirm.step.continueButtonLabel',
    defaultMessage: '!!!Confirm registration Transaction',
    description: 'Label for continue button on the voting add "confirm" step.',
  },
  restartButtonLabel: {
    id: 'voting.votingAdd.confirm.step.restartButtonLabel',
    defaultMessage: '!!!Restart Registration',
    description: 'Label for restart button on the voting add "confirm" step.',
  },
});

type Props = {
  onConfirm: Function,
  onRollback: Function,
  isSubmitting: Boolean,
  transactionError: ?LocalizableError,
  countdownRemaining: number,
};

@observer
export default class VotingAddStepsConfirm extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  render() {
    const { intl } = this.context;
    const {
      onConfirm,
      onRollback,
      isSubmitting,
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
    const restartButtonLabel = intl.formatMessage(messages.restartButtonLabel);

    const className = classNames([
      commonStyles.votingAddSteps,
      styles.votingAddStepsConfirmWrapper,
    ]);

    const contentClassName = classNames([commonStyles.content, styles.content]);

    const countdownDisplay =
      countdownRemaining > 0
        ? `${buttonLabel} [${countdownRemaining} seconds left before confirmation]`
        : buttonLabel;

    return (
      <div className={className}>
        <div className={contentClassName}>
          {!transactionError ? (
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
                <p>{errorMessage}</p>
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
