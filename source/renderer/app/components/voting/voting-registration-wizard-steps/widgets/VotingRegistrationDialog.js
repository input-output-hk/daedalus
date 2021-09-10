// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import type { Node } from 'react';
import classnames from 'classnames';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import { defineMessages, FormattedMessage, intlShape } from 'react-intl';
import styles from './VotingRegistrationDialog.scss';
import Dialog from '../../../widgets/Dialog';
import DialogCloseButton from '../../../widgets/DialogCloseButton';
import DialogBackButton from '../../../widgets/DialogBackButton';
import type { DialogActions } from '../../../widgets/Dialog';

const messages = defineMessages({
  dialogTitle: {
    id: 'voting.votingRegistration.dialog.dialogTitle',
    defaultMessage: '!!!Register for Fund6 voting',
    description: 'Tile "Register to vote" for voting registration',
  },
  subtitle: {
    id: 'voting.votingRegistration.dialog.subtitle',
    defaultMessage: '!!!Step {step} of {stepCount}',
    description: 'Sub title for voting registration',
  },
});

type Props = {
  children: Node,
  stepsList: Array<string>,
  activeStep: number,
  actions: DialogActions,
  onClose: Function,
  onBack?: Function,
  containerClassName?: ?string,
  contentClassName?: ?string,
  hideCloseButton?: boolean,
  hideSteps?: boolean,
};

@observer
export default class VotingRegistrationDialog extends Component<Props> {
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
        values={{ step: activeStep, stepCount: stepsList.length }}
      />
    );

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
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
