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
import { MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE } from '../../../../config/stakingConfig';
import Wallet from '../../../../domains/Wallet';

type Props = InjectedDialogContainerStepProps;
const DefaultProps = InjectedDialogContainerStepDefaultProps;
const messages = defineMessages({
  errorMinRewardFunds: {
    id: 'staking.redeemItnRewards.step1.errorMessage',
    defaultMessage:
      '!!!This wallet does not contain the minimum amount of {calculatedMinRewardsReceiverBalance} ADA which is required to cover the necessary transaction fees. Please select a wallet with <span>a minimum amount of {calculatedMinRewardsReceiverBalance} ADA</span> and click continue.',
    description:
      'errorMinRewardFunds Error Label on the delegation setup "choose wallet" step dialog.',
  },
  errorRestoringWallet: {
    id: 'staking.redeemItnRewards.step1.errorRestoringWallet',
    defaultMessage:
      '!!!This wallet can’t be used for rewards redemption while it’s being synced.',
    description:
      'RestoringWallet Error Label on the rewards redemption setup "choose wallet" step dialog.',
  },
});

@inject('stores', 'actions')
@observer
class Step1ConfigurationContainer extends Component<Props> {
  static defaultProps = DefaultProps;
  onWalletAcceptable = (walletAmount?: BigNumber) => {
    const minRewardsFunds = new BigNumber(
      MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    return walletAmount && walletAmount.gte(minRewardsFunds);
  };

  render() {
    const { actions, stores, onBack, onClose } = this.props;
    const { app, staking, wallets } = stores;
    const { allWallets } = wallets;
    const {
      redeemWallet,
      isCalculatingReedemFees,
      redeemRecoveryPhrase,
    } = staking;
    const { openExternalLink } = app;
    const {
      onConfigurationContinue,
      onCalculateRedeemWalletFees,
    } = actions.staking;
    const selectedWalletId = get(redeemWallet, 'id', null);
    const selectedWallet: Wallet | null | undefined = allWallets.find(
      (current: Wallet) => current && current.id === selectedWalletId
    );
    const { amount, isRestoring } = selectedWallet || {};
    let errorMessage = null;

    if (selectedWallet && !this.onWalletAcceptable(amount)) {
      // Wallet is restoring
      if (isRestoring) errorMessage = messages.errorRestoringWallet;
      // Wallet balance < min rewards redemption funds
      else errorMessage = messages.errorMinRewardFunds;
    }

    return (
      <Step1ConfigurationDialog
        error={errorMessage}
        isCalculatingReedemFees={isCalculatingReedemFees}
        mnemonicValidator={isValidMnemonic}
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onBack={onBack}
        onClose={onClose}
        onContinue={onConfigurationContinue.trigger}
        onSelectWallet={(walletId, recoveryPhrase) =>
          onCalculateRedeemWalletFees.trigger({
            walletId,
            recoveryPhrase,
          })
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

export default Step1ConfigurationContainer;
