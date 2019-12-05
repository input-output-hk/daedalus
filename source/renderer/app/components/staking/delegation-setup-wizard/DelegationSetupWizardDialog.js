// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { BigNumber } from 'bignumber.js';
import DelegationStepsActivationDialog from './DelegationStepsActivationDialog';
import DelegationStepsChooseWalletDialog from './DelegationStepsChooseWalletDialog';
import DelegationStepsConfirmationDialog from './DelegationStepsConfirmationDialog';
import DelegationStepsIntroDialog from './DelegationStepsIntroDialog';
import DelegationStepsNotAvailableDialog from './DelegationStepsNotAvailableDialog';
import DelegationStepsChooseStakePoolDialog from './DelegationStepsChooseStakePoolDialog';
import StakePool from '../../../domains/StakePool';
import Wallet from '../../../domains/Wallet';

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
  isWalletAcceptable: Function,
  stepsList: Array<string>,
  wallets: Array<Wallet>,
  minDelegationFunds: number,
  stakePoolsDelegatingList: Array<StakePool>,
  stakePoolsList: Array<StakePool>,
  onOpenExternalLink: Function,
  getPledgeAddressUrl: Function,
  currentTheme: string,
  selectedWalletId: string,
  selectedPool: ?StakePool,
  getStakePoolById: Function,
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
      getPledgeAddressUrl,
      currentTheme,
      selectedWalletId,
      selectedPool,
      isWalletAcceptable,
      getStakePoolById,
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
            numberOfStakePools={stakePoolsList.length}
            stepsList={stepsList}
            wallets={wallets}
            minDelegationFunds={minDelegationFunds}
            selectedWalletId={selectedWalletId}
            onBack={onBack}
            onClose={onClose}
            onSelectWallet={onSelectWallet}
            isWalletAcceptable={isWalletAcceptable}
            getStakePoolById={getStakePoolById}
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
            getPledgeAddressUrl={getPledgeAddressUrl}
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
