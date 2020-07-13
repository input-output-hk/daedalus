// @flow
import { computed, action, observable, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import { orderBy, find, map, get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import {
  STAKE_POOL_TRANSACTION_CHECK_INTERVAL,
  STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT,
  STAKE_POOLS_INTERVAL,
  STAKE_POOLS_FAST_INTERVAL,
  REDEEM_ITN_REWARDS_STEPS as steps,
} from '../config/stakingConfig';
import type {
  Reward,
  RewardForIncentivizedTestnet,
  JoinStakePoolRequest,
  GetDelegationFeeRequest,
  QuitStakePoolRequest,
} from '../api/staking/types';
import type { RedeemItnRewardsStep } from '../types/stakingTypes';
import Wallet from '../domains/Wallet';
import StakePool from '../domains/StakePool';
import { TransactionStates } from '../domains/WalletTransaction';
import LocalizableError from '../i18n/LocalizableError';
import REWARDS from '../config/stakingRewards.dummy.json';

export default class StakingStore extends Store {
  @observable isDelegationTransactionPending = false;
  @observable fetchingStakePoolsFailed = false;
  @observable isStakingExperimentRead: boolean = false;

  /* ----------  Redeem ITN Rewards  ---------- */
  @observable redeemStep: ?RedeemItnRewardsStep = null;
  @observable redeemWallet: ?Wallet = null;
  @observable walletName: ?string = null;
  @observable redeemError: ?LocalizableError = null;
  @observable rewardsTotal: number = 0;
  @observable transactionFees: number = 0;
  @observable finalTotal: number = 0;
  @observable isSubmittingReedem: boolean = false;
  @observable stakingSuccess: ?boolean = null;
  @observable stakingFailure: number = 0;

  pollingStakePoolsInterval: ?IntervalID = null;
  refreshPolling: ?IntervalID = null;
  delegationCheckTimeInterval: ?IntervalID = null;

  startDateTime: string = '2020-07-29T00:00:00.161Z';
  decentralizationProgress: number = 10;
  adaValue: BigNumber = new BigNumber(82650.15);
  percentage: number = 14;

  _delegationFeeCalculationWalletId: ?string = null;

  setup() {
    const { staking: actions } = this.actions;

    // Redeem ITN Rewards actions
    actions.onRedeemStart.listen(this._onRedeemStart);
    actions.onConfigurationContinue.listen(this._onConfigurationContinue);
    actions.onSelectRedeemWallet.listen(this._onSelectRedeemWallet);
    actions.onConfirmationContinue.listen(this._onConfirmationContinue);
    actions.onResultContinue.listen(this._onResultContinue);
    actions.closeRedeemDialog.listen(this._closeRedeemDialog);

    const { isIncentivizedTestnet, isShelleyTestnet } = global;
    if (isIncentivizedTestnet || isShelleyTestnet) {
      // Set initial fetch interval to 1 second
      this.refreshPolling = setInterval(
        this.getStakePoolsData,
        STAKE_POOLS_FAST_INTERVAL
      );
      actions.goToStakingInfoPage.listen(this._goToStakingInfoPage);
      actions.goToStakingDelegationCenterPage.listen(
        this._goToStakingDelegationCenterPage
      );
      actions.joinStakePool.listen(this._joinStakePool);
      actions.quitStakePool.listen(this._quitStakePool);
      actions.fakeStakePoolsLoading.listen(this._setFakePoller);
    }
  }

  // REQUESTS
  @observable joinStakePoolRequest: Request<JoinStakePoolRequest> = new Request(
    this.api.ada.joinStakePool
  );
  @observable quitStakePoolRequest: Request<QuitStakePoolRequest> = new Request(
    this.api.ada.quitStakePool
  );
  @observable stakePoolsRequest: Request<Array<StakePool>> = new Request(
    this.api.ada.getStakePools
  );
  @observable calculateDelegationFeeRequest: Request<BigNumber> = new Request(
    this.api.ada.calculateDelegationFee
  );
  // @REDEEM TODO: Proper type it when the API endpoint is implemented.
  @observable submitRedeemItnRewardsRequest: Request<any> = new Request(
    this.api.ada.submitRedeemItnRewards
  );

  // =================== PUBLIC API ==================== //

  @action _joinStakePool = async (request: JoinStakePoolRequest) => {
    const { walletId, stakePoolId, passphrase } = request;

    // Set join transaction in "PENDING" state
    this.isDelegationTransactionPending = true;

    try {
      const joinTransaction = await this.joinStakePoolRequest.execute({
        walletId,
        stakePoolId,
        passphrase,
      });
      // Start interval to check transaction state every second
      this.delegationCheckTimeInterval = setInterval(
        this.checkDelegationTransaction,
        STAKE_POOL_TRANSACTION_CHECK_INTERVAL,
        { transactionId: joinTransaction.id, walletId }
      );

      // Reset transaction state check interval after 30 seconds
      setTimeout(() => {
        this.resetStakePoolTransactionChecker();
      }, STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT);
    } catch (error) {
      this.resetStakePoolTransactionChecker();
      throw error;
    }
  };

  @action _quitStakePool = async (request: QuitStakePoolRequest) => {
    const { walletId, passphrase } = request;

    // Set quit transaction in "PENDING" state
    this.isDelegationTransactionPending = true;

    try {
      const quitTransaction = await this.quitStakePoolRequest.execute({
        walletId,
        passphrase,
      });
      // Start interval to check transaction state every second
      this.delegationCheckTimeInterval = setInterval(
        this.checkDelegationTransaction,
        STAKE_POOL_TRANSACTION_CHECK_INTERVAL,
        { transactionId: quitTransaction.id, walletId }
      );

      // Reset transaction state check interval after 30 seconds
      setTimeout(() => {
        this.resetStakePoolTransactionChecker();
      }, STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT);
    } catch (error) {
      this.resetStakePoolTransactionChecker();
      throw error;
    }
  };

  // Check stake pool transaction state and reset pending state when transction is "in_ledger"
  @action checkDelegationTransaction = (request: {
    transactionId: string,
    walletId: string,
  }) => {
    const { transactionId, walletId } = request;
    const recenttransactionsResponse = this.stores.transactions._getTransactionsRecentRequest(
      walletId
    ).result;
    const recentTransactions = recenttransactionsResponse
      ? recenttransactionsResponse.transactions
      : [];

    // Return stake pool transaction when state is not "PENDING"
    const stakePoolTransaction = find(
      recentTransactions,
      transaction =>
        transaction.id === transactionId &&
        transaction.state === TransactionStates.OK
    );

    if (stakePoolTransaction) {
      this.resetStakePoolTransactionChecker();
    }
  };

  // Reset "PENDING" state, transaction state check poller and refresh wallets data
  @action resetStakePoolTransactionChecker = () => {
    if (this.delegationCheckTimeInterval) {
      clearInterval(this.delegationCheckTimeInterval);
      this.delegationCheckTimeInterval = null;
    }
    this.stores.wallets.refreshWalletsData();
    this.isDelegationTransactionPending = false;
  };

  @action markStakingExperimentAsRead = () => {
    this.isStakingExperimentRead = true;
  };

  calculateDelegationFee = async (
    delegationFeeRequest: GetDelegationFeeRequest
  ): ?BigNumber => {
    const { walletId } = delegationFeeRequest;
    const wallet = this.stores.wallets.getWalletById(walletId);
    this._delegationFeeCalculationWalletId = walletId;

    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }

    if (this.calculateDelegationFeeRequest.isExecuting) {
      await this.calculateDelegationFeeRequest;
    }

    try {
      const delegationFee: BigNumber = await this.calculateDelegationFeeRequest.execute(
        { ...delegationFeeRequest }
      ).promise;

      if (this._delegationFeeCalculationWalletId !== walletId) {
        return null;
      }

      return delegationFee;
    } catch (error) {
      throw error;
    }
  };

  // GETTERS

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @computed get stakePools(): Array<StakePool> {
    return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
  }

  @computed get recentStakePools(): Array<StakePool> {
    const delegatedStakePools = [];
    map(this.stores.wallets.all, wallet => {
      const hasPendingDelegations =
        wallet.pendingDelegations && wallet.pendingDelegations.length > 0;
      let lastDelegatedStakePoolId = wallet.delegatedStakePoolId;
      if (hasPendingDelegations) {
        lastDelegatedStakePoolId = wallet.lastDelegationStakePoolId;
      }
      if (lastDelegatedStakePoolId) {
        const delegatingStakePoolExistInList = find(
          delegatedStakePools,
          delegatedStakePool =>
            delegatedStakePool.id === lastDelegatedStakePoolId
        );
        if (!delegatingStakePoolExistInList) {
          const delegatingStakePool = find(
            this.stakePools,
            stakePool => stakePool.id === lastDelegatedStakePoolId
          );
          if (delegatingStakePool)
            delegatedStakePools.push(delegatingStakePool);
        }
      }
    });
    const orderedStakePools = orderBy(delegatedStakePools, 'ranking', 'asc');
    return orderedStakePools;
  }

  @computed get isStakingDelegationCountdown(): boolean {
    return this.currentRoute === ROUTES.STAKING.COUNTDOWN;
  }

  @computed get rewards(): Array<Reward> {
    return REWARDS;
  }

  @computed
  get rewardsForIncentivizedTestnet(): Array<RewardForIncentivizedTestnet> {
    const { wallets } = this.stores;
    return wallets.allWallets.map(
      this._transformWalletToRewardForIncentivizedTestnet
    );
  }

  @action showCountdown(): boolean {
    return new Date(this.startDateTime).getTime() - new Date().getTime() > 0;
  }

  @action getStakePoolsData = async () => {
    const { isConnected } = this.stores.networkStatus;
    if (!isConnected) return;
    try {
      await this.stakePoolsRequest.execute().promise;
      if (this.refreshPolling) this._resetPolling(false);
    } catch (error) {
      if (!this.refreshPolling) {
        this._resetPolling(true);
      }
    }
  };

  @action _resetPolling = (fetchFailed: boolean) => {
    if (fetchFailed) {
      this.fetchingStakePoolsFailed = true;
      clearInterval(this.pollingStakePoolsInterval);
      this.pollingStakePoolsInterval = null;
      this.refreshPolling = setInterval(
        this.getStakePoolsData,
        STAKE_POOLS_FAST_INTERVAL
      );
    } else {
      this.fetchingStakePoolsFailed = false;
      clearInterval(this.refreshPolling);
      this.refreshPolling = null;
      if (!this.pollingStakePoolsInterval) {
        this.pollingStakePoolsInterval = setInterval(
          this.getStakePoolsData,
          STAKE_POOLS_INTERVAL
        );
      }
    }
  };

  // For testing only
  @action _setFakePoller = (forceLoading: boolean) => {
    const { stores, environment } = this;
    const { networkStatus, wallets } = stores;
    const { isConnected } = networkStatus;
    const { _pollingBlocked } = wallets;

    // Enable faker only for development node (NODE_ENV = 'development')
    if (environment.isDev || environment.isTest) {
      if (forceLoading) {
        // Reset all staking pollers
        if (this.refreshPolling) {
          clearInterval(this.refreshPolling);
          this.refreshPolling = null;
        }
        if (this.pollingStakePoolsInterval) {
          clearInterval(this.pollingStakePoolsInterval);
          this.pollingStakePoolsInterval = null;
        }
        this.fetchingStakePoolsFailed = true;
        return;
      }

      // Regular fetching way with faked response that throws error.
      if ((_pollingBlocked || !isConnected) && !this.refreshPolling) {
        this._resetPolling(true);
        return;
      }

      try {
        throw new Error('Faked "Stake pools" fetch error');
      } catch (error) {
        if (!this.refreshPolling) {
          this._resetPolling(true);
        }
      }
    }
  };

  // For testing only
  @action _setFakedStakePools = () => {
    if (this.environment.isDev) {
      if (this.refreshPolling) {
        clearInterval(this.refreshPolling);
        this.refreshPolling = null;
      }
      if (this.pollingStakePoolsInterval) {
        clearInterval(this.pollingStakePoolsInterval);
        this.pollingStakePoolsInterval = null;
      }
      const newStakePools = [
        this.stakePoolsRequest.result[1],
        this.stakePoolsRequest.result[2],
      ];
      this.stakePoolsRequest.reset();
      this.stakePoolsRequest.result = newStakePools;
    }
  };

  /* =================================================
  =            Redeem ITN Rewards - Begin            =
  ================================================= */

  get nextStep() {
    return {
      configuration: steps.CONFIRMATION,
      confirmation: steps.RESULT,
      result: steps.RESULT,
    };
  }

  get prevStep() {
    return {
      configuration: steps.CONFIRMATION,
      confirmation: steps.CONFIGURATION,
      result: steps.CONFIGURATION,
    };
  }

  get redeemActions() {
    return {
      configuration: this._goToConfigurationStep,
      confirmation: this._goToConfirmationStep,
      result: this._goToResultStep,
    };
  }

  @action _goToConfigurationStep = () => {
    this.redeemStep = steps.CONFIGURATION;
  };

  @action _goToConfirmationStep = () => {
    this.redeemStep = steps.CONFIRMATION;
  };

  @action _goToResultStep = () => {
    this.redeemStep = steps.RESULT;
  };

  @action _onSelectRedeemWallet = async ({
    walletId,
  }: {
    walletId: string,
  }) => {
    this.redeemWallet = this.stores.wallets.getWalletById(walletId);
  };

  @action _onRedeemStart = () => {
    this.redeemStep = steps.CONFIGURATION;
  };

  @action _onConfigurationContinue = async ({
    recoveryPhrase,
  }: {
    recoveryPhrase: Array<string>,
  }) => {
    this.isSubmittingReedem = true;
    const { redeemWallet } = this;
    if (!redeemWallet) throw new Error('Redeem wallet required');
    try {
      const {
        rewardsTotal,
        transactionFees,
        finalTotal,
      }: any = await this.submitRedeemItnRewardsRequest.execute({
        walletId: redeemWallet.id,
        recoveryPhrase,
      });
      runInAction('Go to the Confirmation step', () => {
        this.isSubmittingReedem = false;
        this.stakingSuccess = true;
        this.rewardsTotal = rewardsTotal;
        this.transactionFees = transactionFees;
        this.finalTotal = finalTotal;
        this.redeemStep = steps.CONFIRMATION;
      });
    } catch (error) {
      runInAction(() => {
        this._resetRedeemItnRewards();
        this.stakingSuccess = false;
        this.redeemError = error;
        throw error;
      });
    }
  };

  @action _onConfirmationContinue = ({
    spendingPassword,
  }: {
    spendingPassword: string,
  }) => {
    // @REDEEM TODO: Remove when the API endpoint is implemented
    if (spendingPassword === 'FailureErr1') this.stakingFailure = 1;
    else if (spendingPassword === 'FailureErr2') this.stakingFailure = 2;
    else if (spendingPassword === 'FailureErr3') this.stakingFailure = 3;
    else {
      this.stakingFailure = 0;
      this.stakingSuccess = true;
    }
    if (this.stakingFailure > 0) {
      this.stakingSuccess = false;
    }

    this.redeemStep = steps.RESULT;
  };

  @action _onResultContinue = () => {
    if (!this.redeemWallet) throw new Error('Redeem wallet require');
    const { id } = this.redeemWallet;
    this.stores.wallets.goToWalletRoute(id);
    this.redeemStep = null;
    this._resetRedeemItnRewards();
  };

  @action _resetRedeemItnRewards = () => {
    this.isSubmittingReedem = false;
    this.stakingSuccess = null;
    this.redeemWallet = null;
    this.rewardsTotal = 0;
    this.transactionFees = 0;
    this.finalTotal = 0;
    this.stakingFailure = 0;
  };

  @action _closeRedeemDialog = () => {
    this._resetRedeemItnRewards();
    this.redeemStep = null;
  };

  /* ====  End of Redeem ITN Rewards  ===== */

  _goToStakingInfoPage = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.INFO,
    });
  };

  _goToStakingDelegationCenterPage = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.DELEGATION_CENTER,
    });
  };

  _transformWalletToRewardForIncentivizedTestnet = (inputWallet: Wallet) => {
    const { name: wallet, isRestoring, reward, syncState } = inputWallet;
    const syncingProgress = get(syncState, 'progress.quantity', '');
    return { wallet, reward, isRestoring, syncingProgress };
  };

  getStakePoolById = (stakePoolId: string) =>
    this.stakePools.find(({ id }: StakePool) => id === stakePoolId);
}
