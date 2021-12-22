import React, { Component } from 'react';
import { defineMessages, intlShape } from 'react-intl';
import { Stepper } from 'react-polymorph/lib/components/Stepper';
import { StepperSkin } from 'react-polymorph/lib/skins/simple/StepperSkin';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletRestoreSteps.scss' or ... Remove this comment to see the full error message
import styles from './WalletRestoreSteps.scss';
import { RESTORE_WALLET_STEPS } from '../../../../config/walletRestoreConfig';
import type { RestoreWalletStep } from '../../../../types/walletRestoreTypes';

type Props = {
  stepNumber: number;
};
const messages = defineMessages({
  typeStep: {
    id: 'wallet.restore.dialog.typeStep',
    defaultMessage: '!!!Type',
    description: 'Step "Type" in the wallet restore dialog.',
  },
  mnemonicsStep: {
    id: 'wallet.restore.dialog.mnemonicsStep',
    defaultMessage: '!!!Recovery Phrase',
    description: 'Step "Recovery Phrase" in the wallet restore dialog.',
  },
  configurationStep: {
    id: 'wallet.restore.dialog.configurationStep',
    defaultMessage: '!!!Configuration',
    description: 'Step "Configuration" in the wallet restore dialog.',
  },
});
export default class WalletRestoreSteps extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  get filteredSteps(): Array<RestoreWalletStep> {
    return RESTORE_WALLET_STEPS.filter((stepId) => stepId !== 'success');
  }

  get stepsList() {
    return (this as any).filteredSteps.map((stepId) =>
      this.context.intl.formatMessage(messages[`${stepId}Step`])
    );
  }

  render() {
    const { stepNumber } = this.props;
    const currentStep = stepNumber + 1;
    return (
      <div className={styles.component}>
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
