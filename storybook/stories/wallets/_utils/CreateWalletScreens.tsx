import React, { Component } from 'react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import { CREATE_WALLET_STEPS } from '../../../../source/renderer/app/config/walletsConfig';
// Screens
import InstructionsDialog from '../../../../source/renderer/app/components/wallet/wallet-create/InstructionsDialog';
import TemplateDialog from '../../../../source/renderer/app/components/wallet/wallet-create/TemplateDialog';
import MnemonicsDialog from '../../../../source/renderer/app/components/wallet/wallet-create/MnemonicsDialog';
import ValidateDialog from '../../../../source/renderer/app/components/wallet/wallet-create/ValidateDialog';
import HashDialog from '../../../../source/renderer/app/components/wallet/wallet-create/HashDialog';
import ConfigDialog from '../../../../source/renderer/app/components/wallet/wallet-create/ConfigDialog';

type State = {
  currentStep: number;
};
export default class CreateWalletScreens extends Component<any, State> {
  state = {
    currentStep: 0,
  };

  get dialogs() {
    return {
      instructions: InstructionsDialog,
      template: TemplateDialog,
      mnemonics: MnemonicsDialog,
      validate: ValidateDialog,
      hashImage: HashDialog,
      config: ConfigDialog,
    };
  }

  get dialogProps() {
    return {
      instructions: {
        isVideoWatched: boolean('isVideoWatched', false),
      },
      template: {},
      mnemonics: {},
      validate: {},
      hashImage: {},
      config: {},
    };
  }

  onContinue = () => {
    const { currentStep } = this.state;
    let nextStep = currentStep + 1;
    if (nextStep > CREATE_WALLET_STEPS.length - 1) nextStep = 0;
    this.setState({
      currentStep: nextStep,
    });
  };

  render() {
    const { currentStep } = this.state;
    const stepId = CREATE_WALLET_STEPS[currentStep];
    const dialogProps = this.dialogProps[stepId];
    const Dialog = this.dialogs[stepId];
    return (
      <Dialog
        onContinue={this.onContinue}
        onClose={action('onClose')}
        {...dialogProps}
      />
    );
  }
}
