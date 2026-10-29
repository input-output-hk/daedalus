import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import type { InjectedProps } from '../../types/injectedPropsType';
import {
  DECIMAL_PLACES_IN_ADA,
  MAX_INTEGER_PLACES_IN_ADA,
  LOVELACES_PER_ADA,
} from '../../config/numbersConfig';
import WalletSendForm, {
  FormData,
} from '../../components/wallet/WalletSendForm';
import { WalletSendConfirmationDialogView } from './dialogs/send-confirmation/SendConfirmation.view';
import WalletTokenPicker from '../../components/wallet/tokens/wallet-token-picker/WalletTokenPicker';
import { WALLET_ASSETS_ENABLED } from '../../config/walletsConfig';
import Asset from '../../domains/Asset';
import type { ApiTokens } from '../../api/assets/types';
import { getNonZeroAssetTokens } from '../../utils/assets';
import {
  withAnalytics,
  WithAnalyticsTrackerProps,
} from '../../components/analytics/withAnalytics';
import { CoinSelectionsResponse } from '../../api/transactions/types';
import LocalizableError from '../../i18n/LocalizableError';
import ApiError from '../../domains/ApiError';
import { logger } from '../../utils/logging';
import { balancesFor } from '../../utils/governance/rewardWithdrawal';
import type { WithdrawalDecision } from '../../utils/governance/rewardWithdrawal';
import { messages as apiErrorMessages } from '../../api/errors';
import { ROUTES } from '../../routes-config';
import type { RewardsControl } from '../../components/wallet/WalletSendForm';

// The ids of the errors a payment short of funds raises.
const NOT_ENOUGH_FUNDS_ERROR_IDS: Array<string> = [
  apiErrorMessages.notEnoughFundsForTransaction.id,
  apiErrorMessages.cannotCoverFee.id,
  apiErrorMessages.notEnoughMoney.id,
];

type Props = InjectedProps & WithAnalyticsTrackerProps;
type State = {
  confirmationDialogData: Omit<FormData, 'coinSelection'> | null;
  submitError: LocalizableError | ApiError | null;
  // The user's choice for the transaction being prepared; it holds for one
  // wallet and one transaction.
  withdrawRewardsChoice: { walletId: string; value: boolean } | null;
};

@inject('stores', 'actions')
@observer
class WalletSendPage extends Component<Props, State> {
  static defaultProps = {
    actions: null,
    stores: null,
  };

  state: State = {
    confirmationDialogData: null,
    submitError: null,
    withdrawRewardsChoice: null,
  };

  // The control's value: this transaction's choice when it names the wallet,
  // otherwise the wallet's effective setting.
  withdrawRewardsFor = (walletId: string): boolean => {
    const { withdrawRewardsChoice } = this.state;
    return withdrawRewardsChoice?.walletId === walletId
      ? withdrawRewardsChoice.value
      : this.props.stores.walletSettings.withdrawRewardsFor(walletId);
  };

  setWithdrawRewardsChoice = (walletId: string, value: boolean) =>
    this.setState({ withdrawRewardsChoice: { walletId, value } });

  clearWithdrawRewardsChoice = () => {
    if (this.state.withdrawRewardsChoice) {
      this.setState({ withdrawRewardsChoice: null });
    }
  };

  delegateRewards = (walletId: string) => {
    const { stores, actions } = this.props;
    stores.governance.setDelegationNavState({ selectedWalletId: walletId });
    actions.router.goToRoute.trigger({ route: ROUTES.GOVERNANCE.DELEGATE });
  };

  calculateTransactionFee = async (params: {
    walletId: string;
    address: string;
    amount: number;
    isHardwareWallet: boolean;
    selectedAssets?: ApiTokens;
  }) => {
    const withdrawalDecision = this.props.stores.wallets.withdrawalDecisionFor(
      params.walletId,
      'payment',
      { withdrawRewards: this.withdrawRewardsFor(params.walletId) }
    );
    try {
      return await this.estimate(params, withdrawalDecision);
    } catch (error) {
      // A payment the user chose to send without the rewards cannot count
      // them, so a shortfall reads differently from an ordinary one.
      if (
        withdrawalDecision.reason === 'declined' &&
        error != null &&
        NOT_ENOUGH_FUNDS_ERROR_IDS.includes((error as { id?: string }).id)
      ) {
        throw new ApiError().result('notEnoughFundsRewardsNotWithdrawn');
      }
      throw error;
    }
  };

  estimate = async (
    params: {
      walletId: string;
      address: string;
      amount: number;
      isHardwareWallet: boolean;
      selectedAssets?: ApiTokens;
    },
    withdrawalDecision: WithdrawalDecision
  ) => {
    const { walletId, address, amount, isHardwareWallet, selectedAssets } =
      params;
    if (this.state.submitError) this.setState({ submitError: null });
    const { withdrawal } = withdrawalDecision;

    if (isHardwareWallet) {
      const coinSelection: CoinSelectionsResponse =
        await this.props.stores.hardwareWallets.selectCoins({
          walletId,
          address,
          amount,
          assets: selectedAssets,
          withdrawal,
        });
      return {
        fee: coinSelection.fee,
        coinSelection,
        withdrawalDecision,
      };
    }

    const { fee, minimumAda } =
      await this.props.stores.transactions.calculateTransactionFee({
        walletId,
        address,
        amount,
        assets: selectedAssets,
        withdrawal,
      });

    return {
      fee,
      minimumAda,
      withdrawalDecision,
    };
  };

  // A failure that carries a message id was raised before the approval dialog
  // opened, so nothing else shows it. Any other failure is an outcome the
  // approval dialog has already shown.
  showSubmitError = (error: unknown) => {
    if (
      error != null &&
      typeof error === 'object' &&
      typeof (error as { id?: unknown }).id === 'string'
    ) {
      this.setState({ submitError: error as LocalizableError | ApiError });
      return;
    }
    logger.debug('WalletSendPage::submit approval outcome', { error });
  };

  submit = async (
    isHardwareWallet: boolean,
    walletId: string,
    { coinSelection, withdrawalDecision, ...data }: FormData
  ) => {
    const wallet = this.props.stores.wallets.getWalletById(walletId);
    if (!wallet) throw new Error('Wallet required before sending.');
    if (wallet.isLegacy || !global.cip30Enabled) {
      if (isHardwareWallet) {
        this.props.stores.hardwareWallets.updateTxSignRequest(coinSelection);
      }
      this.setState({
        confirmationDialogData: { ...data, withdrawalDecision },
      });
      this.props.actions.dialogs.open.trigger({
        dialog: WalletSendConfirmationDialogView,
      });
      return;
    }
    const amount = data.amount.times(LOVELACES_PER_ADA).toFixed(0);
    const hasAssetsRemainingAfterTransaction = data.selectedAssets.length
      ? !(
          data.selectedAssets.length === wallet.assets.total.length &&
          data.selectedAssets.every(({ quantity }, index) =>
            quantity.isEqualTo(data.assetsAmounts[index])
          )
        )
      : wallet.assets.total.length > 0;
    if (this.state.submitError) this.setState({ submitError: null });
    try {
      if (isHardwareWallet) {
        this.props.stores.hardwareWallets.updateTxSignRequest(
          coinSelection,
          !!data.isCollateralPreparation,
          { address: data.receiver, amount }
        );
        await this.props.actions.hardwareWallets.sendMoney.trigger();
        this.clearWithdrawRewardsChoice();
        return;
      }
      await this.props.actions.wallets.sendMoney.trigger({
        receiver: data.receiver,
        amount,
        assets: data.selectedAssets,
        assetsAmounts: data.assetsAmounts,
        hasAssetsRemainingAfterTransaction,
        isCollateralPreparation: data.isCollateralPreparation,
        withdrawal: withdrawalDecision.withdrawal,
      });
      this.clearWithdrawRewardsChoice();
    } catch (error) {
      this.showSubmitError(error);
    }
  };

  openTokenPickerDialog = () => {
    this.props.actions.dialogs.open.trigger({
      dialog: WalletTokenPicker,
    });
  };

  closeTokenPickerDialog = () => {
    const { actions, stores } = this.props;
    if (!stores.uiDialogs.isOpen(WalletTokenPicker)) return;
    actions.dialogs.closeActiveDialog.trigger();
  };

  getAssetByUniqueId = (uniqueId: string, allAssets: Array<Asset>) => {
    return allAssets.find((asset) => asset.uniqueId === uniqueId);
  };

  render() {
    const { stores, actions } = this.props;
    const {
      uiDialogs,
      wallets,
      transactions,
      app,
      profile,
      hardwareWallets,
      assets: assetsStore,
      addresses,
      collateral,
    } = stores;
    const { isValidAddress, isAddressFromSameWallet } = wallets;
    const { validateAmount, validateAssetAmount } = transactions;
    const { hwDeviceStatus } = hardwareWallets;
    const hasAssetsEnabled = WALLET_ASSETS_ENABLED;
    const { all: allAssets, activeAsset, getAsset, favorites } = assetsStore;
    const { unsetActiveAsset } = actions.wallets;
    const selectedAsset = activeAsset
      ? this.getAssetByUniqueId(activeAsset, allAssets)
      : null;
    // Guard against potential null values
    const wallet = wallets.active;
    if (!wallet) throw new Error('Active wallet required for WalletSendPage.');
    const { isHardwareWallet, name: walletName, singleAddressMode } = wallet;
    const walletTokens = wallet.assets.total;
    const assetTokens = getNonZeroAssetTokens(walletTokens, getAsset);
    const totalRawAssets = wallet.assets.total.length;
    const totalAssets = assetTokens.length;
    const hasRawAssets = wallet.assets.total.length > 0;
    const isLoadingAssets = hasRawAssets && totalAssets < totalRawAssets;
    const isCollateralPreparation =
      global.cip30Enabled &&
      collateral.preparationFormActive &&
      collateral.preparation?.walletId === wallet.id;
    const effectiveWithdrawRewards = this.withdrawRewardsFor(wallet.id);
    const decision = wallets.withdrawalDecisionFor(wallet.id, 'payment', {
      withdrawRewards: effectiveWithdrawRewards,
    });
    let rewardsControl: RewardsControl | null = null;
    if (decision.reason === 'ledger_not_delegated') {
      rewardsControl = {
        checked: false,
        disabled: true,
        reward: wallet.reward,
        reason: 'ledger_not_delegated',
      };
    } else if (
      decision.reason !== 'legacy_wallet' &&
      decision.reason !== 'no_reward'
    ) {
      rewardsControl = {
        checked: decision.kind === 'withdraw',
        disabled: false,
        reward: wallet.reward,
      };
    }
    const preparationAddress = singleAddressMode
      ? addresses.active?.id
      : (addresses.active?.id ?? addresses.all[0]?.id);
    return (
      <WalletSendForm
        currencyMaxIntegerDigits={MAX_INTEGER_PLACES_IN_ADA}
        currencyMaxFractionalDigits={DECIMAL_PLACES_IN_ADA}
        currentNumberFormat={profile.currentNumberFormat}
        calculateTransactionFee={(address, amount, selectedAssets) =>
          this.calculateTransactionFee({
            walletId: wallet.id,
            address,
            amount,
            isHardwareWallet,
            selectedAssets,
          })
        }
        walletAmount={balancesFor(wallet, decision.withdrawal).walletBalance}
        rewardsControl={rewardsControl}
        onRewardsChange={(checked) =>
          this.setWithdrawRewardsChoice(wallet.id, checked)
        }
        onDelegateRewards={() => this.delegateRewards(wallet.id)}
        onReset={this.clearWithdrawRewardsChoice}
        validateAmount={validateAmount}
        validateAssetAmount={validateAssetAmount}
        addressValidator={isValidAddress}
        assets={isCollateralPreparation ? [] : assetTokens}
        hasAssets={!isCollateralPreparation && hasAssetsEnabled && hasRawAssets}
        selectedAsset={isCollateralPreparation ? null : selectedAsset}
        isLoadingAssets={isLoadingAssets}
        isDialogOpen={uiDialogs.isOpen}
        isRestoreActive={wallet.isRestoring}
        isHardwareWallet={isHardwareWallet}
        hwDeviceStatus={hwDeviceStatus}
        onSubmit={(data: FormData) =>
          this.submit(isHardwareWallet, wallet.id, data)
        }
        onUnsetActiveAsset={unsetActiveAsset.trigger}
        onExternalLinkClick={app.openExternalLink}
        isAddressFromSameWallet={isAddressFromSameWallet}
        tokenFavorites={favorites}
        walletName={walletName}
        onTokenPickerDialogOpen={this.openTokenPickerDialog}
        onTokenPickerDialogClose={this.closeTokenPickerDialog}
        analyticsTracker={this.props.analyticsTracker}
        confirmationDialogData={this.state.confirmationDialogData}
        submitError={this.state.submitError}
        initialReceiver={
          isCollateralPreparation ? preparationAddress : undefined
        }
        initialAmount={isCollateralPreparation ? '5' : undefined}
        isCollateralPreparation={isCollateralPreparation}
      />
    );
  }
}

export default withAnalytics(WalletSendPage);
