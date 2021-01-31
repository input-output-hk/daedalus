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
  onRollback: Function,
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
      onRollback,
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
            minVotingRegistrationFunds={minVotingRegistrationFunds}
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
            isTransactionPending={isTransactionPending}
            isTransactionConfirmed={isTransactionConfirmed}
            transactionConfirmations={transactionConfirmations}
            transactionError={transactionError}
            onConfirm={onContinue}
            onRollback={onRollback}
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
