// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import Wallet from '../../../domains/Wallet';
import VotingAddStepsChooseWallet from './VotingAddStepsChooseWallet';
import VotingAddStepsEnterPinCode from './VotingAddStepsEnterPinCode';
import VotingAddStepsDeposit from './VotingAddStepsDeposit';
import VotingAddStepsQrCode from './VotingAddStepsQrCode';
import StakePool from '../../../domains/StakePool';
import { BigNumber } from 'bignumber.js';

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
  onConfirm: Function,
  isDisabled: Boolean,
  isSubmitting: Boolean,
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
      onConfirm,
      isDisabled,
      isSubmitting,
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
          />
        );
        break;
      case 4:
        content = (
          <VotingAddStepsQrCode
            onClose={onClose}
            qrCode={'TEST'}
            isSubmitting={isSubmitting}
          />
        );
        break;
    }

    return content;
  }
}
