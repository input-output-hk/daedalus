// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { BigNumber } from 'bignumber.js';
import VotingAddStepsChooseWallet from './VotingAddStepsChooseWallet';
import VotingAddStepsEnterPinCode from './VotingAddStepsEnterPinCode';
import VotingAddStepsDeposit from './VotingAddStepsDeposit';
import VotingAddStepsQrCode from './VotingAddStepsQrCode';
import StakePool from '../../../domains/StakePool';
import LocalizableError from '../../../i18n/LocalizableError';
import Wallet from '../../../domains/Wallet';

type Props = {
  activeStep: number,
  onClose: Function,
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
  onConfirm: Function,
  qrCode: string | null,
  isSubmitting: Boolean,
  error: ?LocalizableError,
};

@observer
export default class VotingAddWizard extends Component<Props> {
  render() {
    const {
      activeStep,
      onClose,
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
      onConfirm,
      qrCode,
      isSubmitting,
      error,
    } = this.props;

    const selectedWalletId = get(selectedWallet, 'id', null);

    let content = null;
    switch (activeStep) {
      case 1:
        content = (
          <VotingAddStepsChooseWallet
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
        content = <VotingAddStepsEnterPinCode onSetPinCode={onSetPinCode} />;
        break;
      case 3:
        content = (
          <VotingAddStepsDeposit
            onConfirm={onConfirm}
            transactionFee={transactionFee}
            transactionFeeError={transactionFeeError}
            error={error}
          />
        );
        break;
      case 4:
        content = (
          <VotingAddStepsQrCode
            onClose={onClose}
            qrCode={qrCode}
            isSubmitting={isSubmitting}
          />
        );
        break;
      default:
        content = <></>;
        break;
    }

    return content;
  }
}
