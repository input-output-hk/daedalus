// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { get } from 'lodash';
import { BigNumber } from 'bignumber.js';
import VotingAddStepsChooseWallet from './VotingAddStepsChooseWallet';
import VotingAddStepsSign from './VotingAddStepsSign';
import VotingAddStepsConfirm from './VotingAddStepsConfirm';
import VotingAddStepsEnterPinCode from './VotingAddStepsEnterPinCode';
import VotingAddStepsQrCode from './VotingAddStepsQrCode';
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
  isSubmitting: Boolean,
  transactionError: ?LocalizableError,
  onRollback: Function,
  countdownRemaining: number,
  onExternalLinkClick: Function,
};

@observer
export default class VotingAddWizard extends Component<Props> {
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
        content = (
          <VotingAddStepsSign
            onConfirm={onSubmit}
            transactionFee={transactionFee}
            transactionFeeError={transactionFeeError}
            onExternalLinkClick={onExternalLinkClick}
          />
        );
        break;
      case 3:
        content = (
          <VotingAddStepsConfirm
            transactionError={transactionError}
            onConfirm={onContinue}
            onRollback={onRollback}
            isSubmitting={isSubmitting}
            countdownRemaining={countdownRemaining}
          />
        );
        break;
      case 4:
        content = <VotingAddStepsEnterPinCode onSetPinCode={onSetPinCode} />;
        break;
      case 5:
        content = <VotingAddStepsQrCode qrCode={qrCode} />;
        break;
      default:
        content = <></>;
        break;
    }

    return content;
  }
}
