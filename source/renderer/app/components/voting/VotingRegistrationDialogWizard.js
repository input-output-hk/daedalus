// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { BigNumber } from 'bignumber.js';
import VotingRegistrationStepsChooseWallet from './voting-registration-wizar-steps/VotingRegistrationStepsChooseWallet';
import VotingRegistrationStepsSign from './voting-registration-wizar-steps/VotingRegistrationStepsSign';
import VotingRegistrationStepsConfirm from './voting-registration-wizar-steps/VotingRegistrationStepsConfirm';
import VotingRegistrationStepsEnterPinCode from './voting-registration-wizar-steps/VotingRegistrationStepsEnterPinCode';
import VotingRegistrationStepsQrCode from './voting-registration-wizar-steps/VotingRegistrationStepsQrCode';
import StakePool from '../../domains/StakePool';
import LocalizableError from '../../i18n/LocalizableError';
import Wallet from '../../domains/Wallet';

type Props = {
  onClose: Function,
  stepsList: Array<string>,
  activeStep: number,
  onContinue: Function,
  onSelectWallet: Function,
  isWalletAcceptable: Function,
  wallets: Array<Wallet>,
  selectedWallet: ?Wallet,
  onSetPinCode: Function,
  minVotingRegistrationFunds: number,
  stakePoolsList: Array<StakePool>,
  getStakePoolById: Function,
  transactionFee: ?BigNumber,
  transactionFeeError: string | Node | null,
  onSubmit: Function,
  qrCode: ?string,
  isTransactionPending: boolean,
  isTransactionConfirmed: boolean,
  transactionConfirmations: number,
  transactionError: ?LocalizableError,
  onRestart: Function,
  onExternalLinkClick: Function,
};

@observer
export default class VotingRegistrationDialogWizard extends Component<Props> {
  render() {
    const {
      onClose,
      stepsList,
      activeStep,
      onContinue,
      onSelectWallet,
      onSetPinCode,
      wallets,
      selectedWallet,
      isWalletAcceptable,
      stakePoolsList,
      minVotingRegistrationFunds,
      getStakePoolById,
      transactionFee,
      transactionFeeError,
      onSubmit,
      qrCode,
      isTransactionPending,
      isTransactionConfirmed,
      transactionConfirmations,
      transactionError,
      onRestart,
      onExternalLinkClick,
    } = this.props;

    const selectedWalletId = get(selectedWallet, 'id', null);

    let content = null;
    switch (activeStep) {
      case 1:
        content = (
          <VotingRegistrationStepsChooseWallet
            onClose={onClose}
            stepsList={stepsList}
            activeStep={activeStep}
            numberOfStakePools={stakePoolsList.length}
            wallets={wallets}
            minVotingRegistrationFunds={minVotingRegistrationFunds}
            selectedWalletId={selectedWalletId}
            onSelectWallet={onSelectWallet}
            isWalletAcceptable={isWalletAcceptable}
            getStakePoolById={getStakePoolById}
          />
        );
        break;
      case 2:
        content = (
          <VotingRegistrationStepsSign
            onClose={onClose}
            stepsList={stepsList}
            activeStep={activeStep}
            transactionFee={transactionFee}
            transactionFeeError={transactionFeeError}
            transactionError={transactionError}
            isSubmitting={isTransactionPending}
            onConfirm={onSubmit}
            onExternalLinkClick={onExternalLinkClick}
          />
        );
        break;
      case 3:
        content = (
          <VotingRegistrationStepsConfirm
            onClose={onClose}
            stepsList={stepsList}
            activeStep={activeStep}
            isTransactionPending={isTransactionPending}
            isTransactionConfirmed={isTransactionConfirmed}
            transactionConfirmations={transactionConfirmations}
            transactionError={transactionError}
            onConfirm={onContinue}
            onRestart={onRestart}
          />
        );
        break;
      case 4:
        content = (
          <VotingRegistrationStepsEnterPinCode
            onSetPinCode={onSetPinCode}
            onClose={onClose}
            stepsList={stepsList}
            activeStep={activeStep}
          />
        );
        break;
      case 5:
        content = (
          <VotingRegistrationStepsQrCode
            qrCode={qrCode}
            onClose={onClose}
            stepsList={stepsList}
            activeStep={activeStep}
          />
        );
        break;
      default:
        content = <></>;
    }

    return content;
  }
}
