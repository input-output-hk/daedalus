// @flow
import { computed, action, observable } from 'mobx';
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
} from '../config/stakingConfig';
import type {
  Reward,
  RewardForIncentivizedTestnet,
  JoinStakePoolRequest,
  GetDelegationFeeRequest,
  QuitStakePoolRequest,
} from '../api/staking/types';
import Wallet from '../domains/Wallet';
import StakePool from '../domains/StakePool';
import { TransactionStates } from '../domains/WalletTransaction';
import REWARDS from '../config/stakingRewards.dummy.json';

export default class StakingStore extends Store {
  @observable isDelegatioTransactionPending = false;
  @observable fetchingStakePoolsFailed = false;

  pollingStakePoolsInterval: ?IntervalID = null;
  refreshPolling: ?IntervalID = null;
  delegationCheckTimeInterval: ?IntervalID = null;

  startDateTime: string = '2019-12-09T00:00:00.161Z';
  decentralizationProgress: number = 10;
  adaValue: BigNumber = new BigNumber(82650.15);
  percentage: number = 14;

  setup() {
    if (global.isIncentivizedTestnet) {
      // Set initial fetch interval to 1 second
      this.refreshPolling = setInterval(
        this.getStakePoolsData,
        STAKE_POOLS_FAST_INTERVAL
      );
      const { staking } = this.actions;
      staking.goToStakingInfoPage.listen(this._goToStakingInfoPage);
      staking.goToStakingDelegationCenterPage.listen(
        this._goToStakingDelegationCenterPage
      );
      staking.joinStakePool.listen(this._joinStakePool);
      staking.quitStakePool.listen(this._quitStakePool);
      staking.fakeStakePoolsLoading.listen(this._setFakePoller);
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
  @observable isStakingExperimentRead: boolean = false;

  // =================== PUBLIC API ==================== //

  @action _joinStakePool = async (request: JoinStakePoolRequest) => {
    const { walletId, stakePoolId, passphrase } = request;

    // Set join transaction in "PENDING" state
    this.isDelegatioTransactionPending = true;

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

      // Reset transtation state check interval after 30 seconds
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
    this.isDelegatioTransactionPending = true;

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

      // Reset transtation state check interval after 30 seconds
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
    this.isDelegatioTransactionPending = false;
  };

  @action markStakingExperimentAsRead = () => {
    this.isStakingExperimentRead = true;
  };

  calculateDelegationFee = async (
    delegationFeeRequest: GetDelegationFeeRequest
  ) => {
    const { walletId } = delegationFeeRequest;
    const wallet = this.stores.wallets.getWalletById(walletId);

    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }

    return this.api.ada.calculateDelegationFee({
      ...delegationFeeRequest,
    });
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

  @action _resetPolling = fetchFailed => {
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
  @action _setFakePoller = forceLoading => {
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
