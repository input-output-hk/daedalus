// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationStepsActivationDialog from './DelegationStepsActivationDialog';
import DelegationStepsChooseWalletDialog from './DelegationStepsChooseWalletDialog';
import DelegationStepsConfirmationDialog from './DelegationStepsConfirmationDialog';
import DelegationStepsIntroDialog from './DelegationStepsIntroDialog';
import DelegationStepsNotAvailableDialog from './DelegationStepsNotAvailableDialog';

type WalletData = {
  isAcceptableSetupWallet: boolean,
  label: string,
  value: string,
};

type Props = {
  activeStep: number,
  isDisabled: boolean,
  minDelegationFunds: number,
  onActivate: Function,
  onBack: Function,
  onClose: Function,
  onConfirm: Function,
  onContinue: Function,
  onLearnMoreClick: Function,
  stepsList: Array<string>,
  wallets: Array<WalletData>,
};

@observer
export default class DelegationSetupWizardDialog extends Component<Props> {
  render() {
    const {
      activeStep,
      isDisabled,
      minDelegationFunds,
      onActivate,
      onBack,
      onClose,
      onConfirm,
      onContinue,
      onLearnMoreClick,
      stepsList,
      wallets,
    } = this.props;

    if (isDisabled) {
      return (
        <DelegationStepsNotAvailableDialog
          minDelegationFunds={minDelegationFunds}
          onClose={onClose}
        />
      );
    }

    let content = null;
    switch (activeStep) {
      case 1:
        content = (
          <DelegationStepsChooseWalletDialog
            stepsList={stepsList}
            wallets={wallets}
            minDelegationFunds={minDelegationFunds}
            onClose={onClose}
            onContinue={onContinue}
            onBack={onBack}
          />
        );
        break;
      case 3:
        content = (
          <DelegationStepsConfirmationDialog
            stepsList={stepsList}
            isSpendingPasswordSet
            onClose={onClose}
            onConfirm={onConfirm}
            onBack={onBack}
          />
        );
        break;
      case 4:
        content = (
          <DelegationStepsActivationDialog
            stepsList={stepsList}
            isSpendingPasswordSet
            onClose={onClose}
            onActivate={onActivate}
            onBack={onBack}
          />
        );
        break;
      default:
        content = (
          <DelegationStepsIntroDialog
            onLearnMoreClick={onLearnMoreClick}
            onClose={onClose}
            onContinue={onContinue}
          />
        );
        break;
    }

    return content;
  }
}
