// @flow
import { computed, action, observable } from 'mobx';
import BigNumber from 'bignumber.js';
import { orderBy, take } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { ROUTES } from '../routes-config';
import {
  RECENT_STAKE_POOLS_COUNT,
  STAKE_POOLS_INTERVAL,
  STAKE_POOLS_FAST_INTERVAL,
} from '../config/stakingConfig';
import type {
  Reward,
  RewardForIncentivizedTestnet,
  JoinStakePoolRequest,
  GetDelegationFeeRequest,
} from '../api/staking/types';
import Wallet from '../domains/Wallet';
import StakePool from '../domains/StakePool';
import REWARDS from '../config/stakingRewards.dummy.json';

export default class StakingStore extends Store {
  STAKE_POOLS_INITIAL_INTERVAL = 1000; // 1 second | unit: 1000 milliseconds
  STAKE_POOLS_REFRESH_INTERVAL = 30 * 60 * 1000; // 30 minutes | unit: milliseconds;
  @observable fetchingStakePoolsFailed = false;

  pollingStakePoolsInterval: ?IntervalID = null;
  refreshPooling: ?IntervalID = null;

  startDateTime: string = '2019-12-09T00:00:00.161Z';
  decentralizationProgress: number = 10;
  adaValue: BigNumber = new BigNumber(82650.15);
  percentage: number = 14;

  setup() {
    // Initial fetch
    this.getStakePoolsData();
    // Set fetch interval to 30 minutes
    this.pollingStakePoolsInterval = setInterval(
      this.getStakePoolsData,
      STAKE_POOLS_INTERVAL
    );
    const { staking } = this.actions;
    staking.goToStakingInfoPage.listen(this._goToStakingInfoPage);
    staking.goToStakingDelegationCenterPage.listen(
      this._goToStakingDelegationCenterPage
    );
    staking.joinStakePool.listen(this._joinStakePool);
  }

  // REQUESTS
  @observable joinStakePoolRequest: Request<JoinStakePoolRequest> = new Request(
    this.api.ada.joinStakePool
  );
  @observable stakePoolsRequest: Request<Array<StakePool>> = new Request(
    this.api.ada.getStakePools
  );

  // =================== PUBLIC API ==================== //

  @action _joinStakePool = async (request: JoinStakePoolRequest) => {
    const { walletId, stakePoolId, passphrase } = request;
    await this.joinStakePoolRequest.execute({
      walletId,
      stakePoolId,
      passphrase,
    });
    this.stores.wallets.refreshWalletsData();
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
    const orderedStakePools = orderBy(this.stakePools, 'ranking', 'asc');
    return take(orderedStakePools, RECENT_STAKE_POOLS_COUNT);
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
    const { stores } = this;
    const { networkStatus, wallets } = stores;
    const { isSynced, isConnected } = networkStatus;
    const { _pollingBlocked } = wallets;

    if (
      (_pollingBlocked || !isSynced || !isConnected) &&
      !this.refreshPooling
    ) {
      this._resetPolling(true);
      return;
    }

    try {
      await this.stakePoolsRequest.execute().promise;
      if (this.refreshPooling) this._resetPolling(false);
    } catch (error) {
      if (!this.refreshPooling) {
        this._resetPolling(true);
      }
    }
  };

  @action _resetPolling = fetchFailed => {
    if (fetchFailed) {
      this.fetchingStakePoolsFailed = true;
      clearInterval(this.pollingStakePoolsInterval);
      this.pollingStakePoolsInterval = null;
      this.refreshPooling = setInterval(
        this.getStakePoolsData,
        STAKE_POOLS_FAST_INTERVAL
      );
    } else {
      this.fetchingStakePoolsFailed = false;
      clearInterval(this.refreshPooling);
      this.refreshPooling = null;
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
    const { isSynced, isConnected } = networkStatus;
    const { _pollingBlocked } = wallets;

    // Enable faker only for development node (NODE_ENV = 'development')
    if (environment.isDev) {
      if (forceLoading) {
        // Reset all staking pollers
        if (this.refreshPooling) {
          clearInterval(this.refreshPooling);
          this.refreshPooling = null;
        }
        if (this.pollingStakePoolsInterval) {
          clearInterval(this.pollingStakePoolsInterval);
          this.pollingStakePoolsInterval = null;
        }
        this.fetchingStakePoolsFailed = true;
        return;
      }

      // Regular fetching way with faked response that throws error.
      if (
        (_pollingBlocked || !isSynced || !isConnected) &&
        !this.refreshPooling
      ) {
        this._resetPolling(true);
        return;
      }

      try {
        throw new Error('Faked "Stake pools" fetch error');
      } catch (error) {
        if (!this.refreshPooling) {
          this._resetPolling(true);
        }
      }
    }
  };

  // For testing only
  @action _setFakedStakePools = () => {
    if (this.environment.isDev) {
      if (this.refreshPooling) {
        clearInterval(this.refreshPooling);
        this.refreshPooling = null;
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
    const { name: wallet, reward } = inputWallet;
    return { wallet, reward };
  };

  getStakePoolById = (stakePoolId: string) =>
    this.stakePools.find(({ id }: StakePool) => id === stakePoolId);
}
