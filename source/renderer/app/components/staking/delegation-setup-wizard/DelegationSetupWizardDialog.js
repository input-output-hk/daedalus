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
import LocalizableError from '../../../i18n/LocalizableError';
import Wallet from '../../../domains/Wallet';
import type { StakePool } from '../../../api/staking/types';

type Props = {
  activeStep: number,
  isDisabled: boolean,
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
  currentTheme: string,
  selectedWallet: ?Wallet,
  selectedPool: ?StakePool,
  stakePoolJoinFee: ?BigNumber,
  isSubmitting: boolean,
  error: ?LocalizableError,
};

@observer
export default class DelegationSetupWizardDialog extends Component<Props> {
  render() {
    const {
      activeStep,
      isDisabled,
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
      isWalletAcceptable,
      stakePoolJoinFee,
      isSubmitting,
      error,
    } = this.props;

    const selectedWalletId = get(selectedWallet, 'id', null);

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
            selectedWalletId={selectedWalletId}
            onBack={onBack}
            onClose={onClose}
            onSelectWallet={onSelectWallet}
            isWalletAcceptable={isWalletAcceptable}
          />
        );
        break;
      case 2:
        content = (
          <DelegationStepsChooseStakePoolDialog
            stepsList={stepsList}
            stakePoolsDelegatingList={stakePoolsDelegatingList}
            stakePoolsList={stakePoolsList}
            selectedWallet={selectedWallet}
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
            transactionFee={stakePoolJoinFee}
            selectedPool={selectedPool}
            selectedWallet={selectedWallet}
            stepsList={stepsList}
            onClose={onClose}
            onConfirm={onConfirm}
            onBack={onBack}
            isSubmitting={isSubmitting}
            error={error}
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
