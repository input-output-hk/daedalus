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
import type { DialogActions } from '../../../widgets/Dialog';

const messages = defineMessages({
  dialogTitle: {
    id: 'voting.votingRegistration.dialog.dialogTitle',
    defaultMessage: '!!!Register for Fund3 voting',
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
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  actions: DialogActions,
  containerClassName?: ?string,
  contentClassName?: ?string,
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
      onClose,
      activeStep,
      stepsList,
      actions,
      containerClassName,
      contentClassName,
    } = this.props;
    const containerStyles = classnames([styles.container, containerClassName]);
    const contentStyles = classnames([styles.content, contentClassName]);

    return (
      <Dialog
        className={styles.component}
        title={intl.formatMessage(messages.dialogTitle)}
        closeOnOverlayClick={false}
        onClose={onClose}
        closeButton={<DialogCloseButton />}
        actions={actions}
      >
        <div className={styles.subtitle}>
          <FormattedMessage
            {...messages.subtitle}
            values={{ step: activeStep, stepCount: stepsList.length }}
          />
        </div>
        <div className={styles.votingRegistrationStepsIndicatorWrapper}>
          <Stepper
            steps={stepsList}
            activeStep={activeStep}
            skin={StepperSkin}
            labelDisabled
          />
        </div>
        <div className={containerStyles}>
          <div className={contentStyles}>{children}</div>
        </div>
      </Dialog>
    );
  }
}
