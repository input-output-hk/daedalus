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
import LocalizableError from '../../../../i18n/LocalizableError';

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

class Step1ConfigurationContainer extends Component<Props> {
  static defaultProps = DefaultProps;
  hasEnoughAdaToCoverFees = (walletAmount?: BigNumber) => {
    const minRewardsFunds = new BigNumber(
      MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE
    );
    return walletAmount && walletAmount.gte(minRewardsFunds);
  };

  getErrorMessage = (wallet?: Wallet): LocalizableError | null => {
    if (!wallet) {
      return null;
    }

    const { amount, isRestoring } = wallet;

    if (isRestoring) {
      return messages.errorRestoringWallet;
    }
    if (!this.hasEnoughAdaToCoverFees(amount)) {
      return messages.errorMinRewardFunds;
    }

    return null;
  };

  render() {
    const { actions, stores, onClose } = this.props;
    const { app, staking, wallets } = stores;
    const { allWallets } = wallets;
    const { redeemWallet, isCalculatingReedemFees, redeemRecoveryPhrase } =
      staking;
    const { openExternalLink } = app;
    const { onConfigurationContinue, onCalculateRedeemWalletFees } =
      actions.staking;
    const selectedWalletId = get(redeemWallet, 'id', null);
    const selectedWallet: Wallet | null | undefined = allWallets.find(
      (current: Wallet) => current && current.id === selectedWalletId
    );
    const errorMessage = this.getErrorMessage(selectedWallet);

    return (
      <Step1ConfigurationDialog
        error={errorMessage}
        isCalculatingReedemFees={isCalculatingReedemFees}
        mnemonicValidator={isValidMnemonic}
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

export default inject(
  'stores',
  'actions'
)(observer(Step1ConfigurationContainer));
