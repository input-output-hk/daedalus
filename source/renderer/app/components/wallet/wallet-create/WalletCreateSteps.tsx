import React, { Component } from 'react';
import { defineMessages, intlShape, FormattedHTMLMessage } from 'react-intl';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletCreateSteps.scss' or i... Remove this comment to see the full error message
import styles from './WalletCreateSteps.scss';
import { CREATE_WALLET_STEPS } from '../../../config/walletsConfig';
import type { RestoreWalletStep } from '../../../types/walletRestoreTypes';

type Props = {
  stepNumber: number;
};
const messages = defineMessages({
  dialogTitle: {
    id: 'wallet.create.dialog.title',
    defaultMessage: '!!!Create a new wallet',
    description: 'Title "Create a new wallet" in the wallet create dialog.',
  },
  stepsCounter: {
    id: 'wallet.create.dialog.stepsCounter',
    defaultMessage: '!!!Step {currentStep} of {totalSteps}',
    description: 'Step couters in the wallet create dialog.',
  },
  instructionsStep: {
    id: 'wallet.create.dialog.instructionsStep',
    defaultMessage: '!!!Instructions',
    description: 'Step "Instructions" in the wallet create dialog.',
  },
  templateStep: {
    id: 'wallet.create.dialog.templateStep',
    defaultMessage: '!!!Template',
    description: 'Step "Template" in the wallet create dialog.',
  },
  mnemonicsStep: {
    id: 'wallet.create.dialog.mnemonicsStep',
    defaultMessage: '!!!Mnemonics',
    description: 'Step "Mnemonics" in the wallet create dialog.',
  },
  validateStep: {
    id: 'wallet.create.dialog.validateStep',
    defaultMessage: '!!!Validate',
    description: 'Step "Validate" in the wallet create dialog.',
  },
  hashImageStep: {
    id: 'wallet.create.dialog.hashImageStep',
    defaultMessage: '!!!Hash & Image',
    description: 'Step "HashImage" in the wallet create dialog.',
  },
  configStep: {
    id: 'wallet.create.dialog.configStep',
    defaultMessage: '!!!Config',
    description: 'Step "Config" in the wallet create dialog.',
  },
});
export default class WalletCreateSteps extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get stepsList(): Array<RestoreWalletStep> {
    return CREATE_WALLET_STEPS.map((stepId) =>
      this.context.intl.formatMessage(messages[`${stepId}Step`])
    );
  }

  render() {
    const { stepNumber } = this.props;
    const currentStep = stepNumber + 1;
    const totalSteps = CREATE_WALLET_STEPS.length;
    return (
      <div>
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
