// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { BigNumber } from 'bignumber.js';
import { get } from 'lodash';
import DelegationStepsActivationDialog from './DelegationStepsActivationDialog';
import DelegationStepsChooseWalletDialog from './DelegationStepsChooseWalletDialog';
import DelegationStepsConfirmationDialog from './DelegationStepsConfirmationDialog';
import DelegationStepsIntroDialog from './DelegationStepsIntroDialog';
import DelegationStepsNotAvailableDialog from './DelegationStepsNotAvailableDialog';
import DelegationStepsChooseStakePoolDialog from './DelegationStepsChooseStakePoolDialog';
import type { StakePool } from '../../../api/staking/types';

type DelegationWalletData = {
  id: string,
  isAcceptableSetupWallet: boolean,
  label: string,
  value: string,
  hasPassword: boolean,
};

type Props = {
  activeStep: number,
  isDisabled: boolean,
  onActivate: Function,
  onBack: Function,
  onClose: Function,
  onConfirm: Function,
  onContinue: Function,
  onLearnMoreClick: Function,
  onSelectWallet: Function,
  onSelectPool: Function,
  stepsList: Array<string>,
  wallets: Array<DelegationWalletData>,
  minDelegationFunds: number,
  stakePoolsDelegatingList: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  currentTheme: string,
  selectedWallet: ?DelegationWalletData,
  selectedPool: ?StakePool,
};

@observer
export default class DelegationSetupWizardDialog extends Component<Props> {
  render() {
    const {
      activeStep,
      isDisabled,
      onActivate,
      onBack,
      onClose,
      onConfirm,
      onContinue,
      onLearnMoreClick,
      onSelectWallet,
      onSelectPool,
      stepsList,
      wallets,
      minDelegationFunds,
      stakePoolsDelegatingList,
      stakePoolsList,
      onOpenExternalLink,
      currentTheme,
      selectedWallet,
      selectedPool,
    } = this.props;

    const selectedWalletHasPassword = get(selectedWallet, 'hasPassword', false);

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
            selectedWallet={selectedWallet}
            onBack={onBack}
            onClose={onClose}
            onSelectWallet={onSelectWallet}
          />
        );
        break;
      case 2:
        content = (
          <DelegationStepsChooseStakePoolDialog
            stepsList={stepsList}
            stakePoolsDelegatingList={stakePoolsDelegatingList}
            stakePoolsList={stakePoolsList}
            onOpenExternalLink={onOpenExternalLink}
            currentTheme={currentTheme}
            selectedPool={selectedPool}
            onClose={onClose}
            onBack={onBack}
            onSelectPool={onSelectPool}
          />
        );
        break;
      case 3:
        content = (
          <DelegationStepsConfirmationDialog
            fees={new BigNumber(0.172081)}
            stepsList={stepsList}
            isSpendingPasswordSet={selectedWalletHasPassword}
            onClose={onClose}
            onConfirm={onConfirm}
            onBack={onBack}
          />
        );
        break;
      case 4:
        content = (
          <DelegationStepsActivationDialog
            amount={new BigNumber(3)}
            fees={new BigNumber(0.172081)}
            stepsList={stepsList}
            isSpendingPasswordSet={selectedWalletHasPassword}
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
