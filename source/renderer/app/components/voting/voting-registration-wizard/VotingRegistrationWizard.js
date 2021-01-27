// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { BigNumber } from 'bignumber.js';
import VotingRegistrationStepsChooseWallet from './VotingRegistrationStepsChooseWallet';
import VotingRegistrationStepsSign from './VotingRegistrationStepsSign';
import VotingRegistrationStepsConfirm from './VotingRegistrationStepsConfirm';
import VotingRegistrationStepsEnterPinCode from './VotingRegistrationStepsEnterPinCode';
import VotingRegistrationStepsQrCode from './VotingRegistrationStepsQrCode';
import StakePool from '../../../domains/StakePool';
import LocalizableError from '../../../i18n/LocalizableError';
import Wallet from '../../../domains/Wallet';

type Props = {
  activeStep: number,
  onContinue: Function,
  onSelectWallet: Function,
  isWalletAcceptable: Function,
  wallets: Array<Wallet>,
  selectedWallet: ?Wallet,
  onSetPinCode: Function,
  minVotingFunds: number,
  stakePoolsList: Array<StakePool>,
  getStakePoolById: Function,
  transactionFee: ?BigNumber,
  transactionFeeError: string | Node | null,
  onSubmit: Function,
  qrCode: string | null,
  isSubmitting: boolean,
  isTransactionApproved: boolean,
  transactionError: ?LocalizableError,
  onRollback: Function,
  countdownRemaining: number,
  onExternalLinkClick: Function,
};

@observer
export default class VotingRegistrationWizard extends Component<Props> {
  render() {
    const {
      activeStep,
      onContinue,
      onSelectWallet,
      onSetPinCode,
      wallets,
      selectedWallet,
      isWalletAcceptable,
      stakePoolsList,
      minVotingFunds,
      getStakePoolById,
      transactionFee,
      transactionFeeError,
      onSubmit,
      qrCode,
      isSubmitting,
      isTransactionApproved,
      transactionError,
      onRollback,
      countdownRemaining,
      onExternalLinkClick,
    } = this.props;

    const selectedWalletId = get(selectedWallet, 'id', null);

    let content = null;
    switch (activeStep) {
      case 1:
        content = (
          <VotingRegistrationStepsChooseWallet
            numberOfStakePools={stakePoolsList.length}
            wallets={wallets}
            minVotingFunds={minVotingFunds}
            selectedWalletId={selectedWalletId}
            onContinue={onContinue}
            onSelectWallet={onSelectWallet}
            isWalletAcceptable={isWalletAcceptable}
            getStakePoolById={getStakePoolById}
          />
        );
        break;
      case 2:
        content = (
          <VotingRegistrationStepsSign
            onConfirm={onSubmit}
            transactionFee={transactionFee}
            transactionFeeError={transactionFeeError}
            onExternalLinkClick={onExternalLinkClick}
          />
        );
        break;
      case 3:
        content = (
          <VotingRegistrationStepsConfirm
            transactionError={transactionError}
            onConfirm={onContinue}
            onRollback={onRollback}
            isSubmitting={isSubmitting}
            isTransactionApproved={isTransactionApproved}
            countdownRemaining={countdownRemaining}
          />
        );
        break;
      case 4:
        content = (
          <VotingRegistrationStepsEnterPinCode onSetPinCode={onSetPinCode} />
        );
        break;
      case 5:
        content = <VotingRegistrationStepsQrCode qrCode={qrCode} />;
        break;
      default:
        content = <></>;
        break;
    }

    return content;
  }
}
