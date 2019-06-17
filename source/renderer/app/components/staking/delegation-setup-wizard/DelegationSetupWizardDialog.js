// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import DelegationStepsNotAvailableDialog from './DelegationStepsNotAvailableDialog';
import DelegationStepsIntroDialog from './DelegationStepsIntroDialog';
import DelegationStepsChooseWalletDialog from './DelegationStepsChooseWalletDialog';

type WalletData = {
  label: string,
  value: string,
  isAcceptableSetupWallet: boolean,
};

type Props = {
  isDisabled: boolean,
  activeStep: number,
  wallets: Array<WalletData>,
  onClose: Function,
  onContinue: Function,
  onBack: Function,
  onLearnMoreClick: Function,
  stepsList: Array<string>,
  minDelegationFunds: number,
};

@observer
export default class DelegationSetupWizardDialog extends Component<Props> {
  render() {
    const {
      isDisabled,
      onClose,
      onContinue,
      onBack,
      activeStep,
      wallets,
      onLearnMoreClick,
      stepsList,
      minDelegationFunds,
    } = this.props;

    let content = null;
    if (isDisabled) {
      content = (
        <DelegationStepsNotAvailableDialog
          minDelegationFunds={minDelegationFunds}
          onClose={onClose}
        />
      );
    } else if (activeStep === 0) {
      content = (
        <DelegationStepsIntroDialog
          onLearnMoreClick={onLearnMoreClick}
          onClose={onClose}
          onContinue={onContinue}
        />
      );
    } else if (activeStep === 1) {
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
    }

    return content;
  }
}
