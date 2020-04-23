// @flow
import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
import styles from './WalletRestoreSteps.scss';
import { RESTORE_WALLET_STEPS } from '../../../../config/walletRestoreConfig';
import type { RestoreWalletStep } from '../../../../types/walletRestoreTypes';

type Props = {
  stepNumber: number,
};

const messages = defineMessages({
  stepsCounter: {
    id: 'wallet.restore.dialog.stepsCounter',
    defaultMessage: '!!!Step {currentStep} of {totalSteps}',
    description: 'Step couters in the wallet create dialog.',
  },
  typeStep: {
    id: 'wallet.restore.dialog.typeStep',
    defaultMessage: '!!!Type',
    description: 'Step "Type" in the wallet create dialog.',
  },
  mnemonicsStep: {
    id: 'wallet.restore.dialog.mnemonicsStep',
    defaultMessage: '!!!Recovery Phrase',
    description: 'Step "Recovery Phrase" in the wallet create dialog.',
  },
  configurationStep: {
    id: 'wallet.restore.dialog.configurationStep',
    defaultMessage: '!!!Configuration',
    description: 'Step "Configuration" in the wallet create dialog.',
  },
});

export default class WalletRestoreSteps extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get filteredSteps(): Array<RestoreWalletStep> {
    return RESTORE_WALLET_STEPS.filter(stepId => stepId !== 'success');
  }

  get stepsList() {
    return (this: any).filteredSteps.map(stepId =>
      this.context.intl.formatMessage(messages[`${stepId}Step`])
    );
  }

  render() {
    const { stepNumber } = this.props;
    const currentStep = stepNumber + 1;
    const totalSteps = this.filteredSteps.length;
    return (
      <div className={styles.component}>
        <div className={styles.stepCounter}>
          <FormattedHTMLMessage
            {...messages.stepsCounter}
            values={{
              currentStep,
              totalSteps,
            }}
          />
        </div>
        <Stepper
          steps={this.stepsList}
          activeStep={currentStep}
          skin={StepperSkin}
          labelDisabled
        />
      </div>
    );
  }
}
