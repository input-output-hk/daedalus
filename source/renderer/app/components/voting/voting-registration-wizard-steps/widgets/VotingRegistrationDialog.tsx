import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import classnames from 'classnames';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { defineMessages, FormattedMessage, intlShape } from 'react-intl';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './VotingRegistrationDialog.scs... Remove this comment to see the full error message
import styles from './VotingRegistrationDialog.scss';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import type { DialogActions } from '../../../widgets/Dialog';
import { NEXT_VOTING_FUND_NUMBER } from '../../../../config/votingConfig';

const messages = defineMessages({
  dialogTitle: {
    id: 'voting.votingRegistration.dialog.dialogTitle',
    defaultMessage: '!!!Register for Fund{nextVotingFundNumber} voting',
    description: 'Tile "Register to vote" for voting registration',
  },
  subtitle: {
    id: 'voting.votingRegistration.dialog.subtitle',
    defaultMessage: '!!!Step {step} of {stepCount}',
    description: 'Sub title for voting registration',
  },
});
type Props = {
  children: Node;
  stepsList: Array<string>;
  activeStep: number;
  actions: DialogActions;
  onClose: (...args: Array<any>) => any;
  onBack?: (...args: Array<any>) => any;
  containerClassName?: string | null | undefined;
  contentClassName?: string | null | undefined;
  hideCloseButton?: boolean;
  hideSteps?: boolean;
};

@observer
class VotingRegistrationDialog extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };
  static defaultProps = {
    children: null,
  };

  render() {
    const { intl } = this.context;
    const {
      children,
      activeStep,
      stepsList,
      actions,
      onClose,
      onBack,
      containerClassName,
      contentClassName,
      hideCloseButton,
      hideSteps,
    } = this.props;
    const containerStyles = classnames([styles.container, containerClassName]);
    const contentStyles = classnames([styles.content, contentClassName]);
    const stepsIndicatorLabel = (
      <FormattedMessage
        {...messages.subtitle}
        values={{
          step: activeStep,
          stepCount: stepsList.length,
        }}
      />
    );
    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle, {
          nextVotingFundNumber: NEXT_VOTING_FUND_NUMBER,
        })}
        subtitle={!hideSteps && stepsIndicatorLabel}
        onClose={onClose}
        closeOnOverlayClick={false}
        closeButton={hideCloseButton ? null : <DialogCloseButton />}
        backButton={onBack && <DialogBackButton onBack={onBack} />}
        actions={actions}
      >
        {!hideSteps && (
          <div className={styles.votingRegistrationStepsIndicatorWrapper}>
            <Stepper
              steps={stepsList}
              activeStep={activeStep}
              skin={StepperSkin}
              labelDisabled
            />
          </div>
        )}
        <div className={containerStyles}>
          <div className={contentStyles}>{children}</div>
        </div>
      </Dialog>
    );
  }
}

export default VotingRegistrationDialog;
