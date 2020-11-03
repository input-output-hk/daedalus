// @flow
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import BigNumber from 'bignumber.js';
import { get } from 'lodash';
import { defineMessages } from 'react-intl';
import Step1ConfigurationDialog from '../../../../components/staking/redeem-itn-rewards/Step1ConfigurationDialog';
import type { InjectedDialogContainerStepProps } from '../../../../types/injectedPropsType';
import { InjectedDialogContainerStepDefaultProps } from '../../../../types/injectedPropsType';
import validWords from '../../../../../../common/config/crypto/valid-words.en';
import { isValidMnemonic } from '../../../../../../common/config/crypto/decrypt';
import { MIN_REWARDS_FUNDS } from '../../../../config/stakingConfig';
import Wallet from '../../../../domains/Wallet';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;

const messages = defineMessages({
  errorMinRewardFunds: {
    id:
      'staking.redeemItnRewards.step1.errorMessage',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {minRewardFunds} ADA which is required to cover the necessary transaction fees. Please select a wallet with <span>a minimum amount of {minRewardFunds} ADA</span> and click continue.',
    description:
      'errorMinRewardFunds Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorMinDelegationFundsRewardsOnly: {
    id:
      'staking.delegationSetup.chooseWallet.step.dialog.errorMinDelegationFundsRewardsOnly',
    defaultMessage:
      '!!!This wallet contains only rewards balances so it cannot be delegated.',
    description:
      'errorMinDelegationFundsRewardsOnly Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorRestoringWallet: {
    id: 'staking.delegationSetup.chooseWallet.step.dialog.errorRestoringWallet',
    defaultMessage:
      '!!!This wallet can’t be used for delegation while it’s being synced.',
    description:
      'RestoringWallet Error Label on the delegation setup "choose wallet" step dialog.',
  },
});

@inject('stores', 'actions')
@observer
export default class Step1ConfigurationContainer extends Component<Props> {
  static defaultProps = DefaultProps;

  onWalletAcceptable = (
    walletAmount?: BigNumber,
    walletReward?: BigNumber = 0
  ) =>
    walletAmount &&
    walletAmount.gte(new BigNumber(MIN_REWARDS_FUNDS)) &&
    !walletAmount.equals(walletReward);

  render() {
    const { onClose, onBack, stores, actions } = this.props;
    const { allWallets } = stores.wallets;
    const {
      redeemWallet,
      configurationStepError,
      isSubmittingReedem,
      redeemRecoveryPhrase,
    } = stores.staking;

    const selectedWalletId = get(redeemWallet, 'id', null);
    const selectedWallet: ?Wallet = allWallets.find(
      (current: Wallet) => current && current.id === selectedWalletId
    );
    const { amount, reward, isRestoring } = selectedWallet || {};
    let errorMessage = configurationStepError;
    if (selectedWallet && !this.onWalletAcceptable(amount, reward)) {
      // Wallet is restoring
      if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet only has Reward balance
      else if (!amount.isZero() && amount.equals(reward))
        errorMessage = messages.errorMinDelegationFundsRewardsOnly;
      // Wallet balance < min delegation funds
      else errorMessage = messages.errorMinRewardFunds;
    }
    const { openExternalLink } = stores.app;
    const { onConfigurationContinue, onSelectRedeemWallet } = actions.staking;
    return (
      <Step1ConfigurationDialog
        error={errorMessage}
        isSubmitting={isSubmittingReedem}
        mnemonicValidator={isValidMnemonic}
        onBack={onBack}
        onClose={onClose}
        onContinue={onConfigurationContinue.trigger}
        onSelectWallet={(walletId) =>
          onSelectRedeemWallet.trigger({ walletId })
        }
        suggestedMnemonics={validWords}
        wallet={redeemWallet}
        wallets={allWallets}
        openExternalLink={openExternalLink}
        recoveryPhrase={redeemRecoveryPhrase}
      />
    );
  }
}
