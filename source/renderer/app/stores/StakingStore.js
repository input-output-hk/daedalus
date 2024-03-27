'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const path_1 = __importDefault(require('path'));
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const routes_config_1 = require('../routes-config');
const numbersConfig_1 = require('../config/numbersConfig');
const stakingConfig_1 = require('../config/stakingConfig');
const Wallet_1 = __importDefault(require('../domains/Wallet'));
const WalletTransaction_1 = require('../domains/WalletTransaction');
const LocalizableError_1 = __importDefault(require('../i18n/LocalizableError'));
const show_file_dialog_channels_1 = require('../ipc/show-file-dialog-channels');
const files_1 = require('../../../common/utils/files');
const analytics_1 = require('../analytics');
class StakingStore extends Store_1.default {
  isDelegationTransactionPending = false;
  fetchingStakePoolsFailed = false;
  selectedDelegationWalletId = null;
  stake = stakingConfig_1.INITIAL_DELEGATION_FUNDS;
  isRanking = false;
  smashServerUrl = null;
  smashServerUrlError = null;
  smashServerLoading = false;
  stakePoolsListViewTooltipVisible = true;
  /* ----------  Redeem ITN Rewards  ---------- */
  redeemStep = null;
  redeemRecoveryPhrase = null;
  redeemWallet = null;
  walletName = null;
  transactionFees = null;
  redeemedRewards = null;
  isSubmittingReedem = false;
  isCalculatingReedemFees = false;
  redeemSuccess = null;
  configurationStepError = null;
  confirmationStepError = null;
  /* ----------  Stake Pools Fetching Tracker  ---------- */
  isFetchingStakePools = false;
  numberOfStakePoolsFetched = 0;
  cyclesWithoutIncreasingStakePools = 0;
  stakingInfoWasOpen = false;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingStakePoolsInterval = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  refreshPolling = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  delegationCheckTimeInterval = null;
  adaValue = new bignumber_js_1.default(82650.15);
  percentage = 14;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  stakePoolsFetchTrackerInterval = null;
  _delegationFeeCalculationWalletId = null;
  setup() {
    const {
      staking: stakingActions,
      networkStatus: networkStatusActions,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Staking... Remove this comment to see the full error message
    } = this.actions;
    this.refreshPolling = setInterval(
      this.getStakePoolsData,
      stakingConfig_1.STAKE_POOLS_FAST_INTERVAL
    );
    // Redeem ITN Rewards actions
    stakingActions.onRedeemStart.listen(this._onRedeemStart);
    stakingActions.onConfigurationContinue.listen(
      this._onConfigurationContinue
    );
    stakingActions.onCalculateRedeemWalletFees.listen(
      this._onCalculateRedeemWalletFees
    );
    stakingActions.onConfirmationContinue.listen(this._onConfirmationContinue);
    stakingActions.onResultContinue.listen(this._onResultContinue);
    stakingActions.closeRedeemDialog.listen(this._closeRedeemDialog);
    stakingActions.goToStakingInfoPage.listen(this._goToStakingInfoPage);
    stakingActions.goToStakingDelegationCenterPage.listen(
      this._goToStakingDelegationCenterPage
    );
    stakingActions.joinStakePool.listen(this._joinStakePool);
    stakingActions.quitStakePool.listen(this._quitStakePool);
    stakingActions.fakeStakePoolsLoading.listen(this._setFakePoller);
    stakingActions.updateDelegatingStake.listen(this._setStake);
    stakingActions.rankStakePools.listen(this._rankStakePools);
    stakingActions.selectSmashServerUrl.listen(this._selectSmashServerUrl);
    stakingActions.resetSmashServerError.listen(this._resetSmashServerError);
    stakingActions.selectDelegationWallet.listen(
      this._setSelectedDelegationWalletId
    );
    stakingActions.requestCSVFile.listen(this._requestCSVFile);
    stakingActions.setStakingInfoWasOpen.listen(this._setStakingInfoWasOpen);
    networkStatusActions.isSyncedAndReady.listen(this._getSmashSettingsRequest);
    // ========== MOBX REACTIONS =========== //
    // @ts-ignore ts-migrate(2339) FIXME: Property 'registerReactions' does not exist on typ... Remove this comment to see the full error message
    this.registerReactions([this._pollOnSync]);
    this._startStakePoolsFetchTracker();
    this._getStakingInfoWasOpen();
    this._getStakePoolsListViewTooltip();
  }
  // REQUESTS
  joinStakePoolRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.joinStakePool
  );
  quitStakePoolRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.quitStakePool
  );
  stakePoolsRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.getStakePools
  );
  calculateDelegationFeeRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.calculateDelegationFee
  );
  // @REDEEM TODO: Proper type it when the API endpoint is implemented.
  getRedeemItnRewardsFeeRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.getRedeemItnRewardsFee
  );
  requestRedeemItnRewardsRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.requestRedeemItnRewards
  );
  getSmashSettingsRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.getSmashSettings
  );
  updateSmashSettingsRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.ada.updateSmashSettings
  );
  // =================== PUBLIC API ==================== //
  _getSmashSettingsRequest = async () => {
    this.smashServerLoading = true;
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    let smashServerUrl = await this.getSmashSettingsRequest.execute();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    const localSmashServer = await this.api.localStorage.getSmashServer();
    // If the server wasn't set, sets it for IOHK
    if (
      !smashServerUrl ||
      smashServerUrl === stakingConfig_1.SMASH_SERVER_INVALID_TYPES.NONE ||
      (smashServerUrl === stakingConfig_1.SMASH_SERVER_TYPES.DIRECT &&
        localSmashServer !== stakingConfig_1.SMASH_SERVER_TYPES.DIRECT)
    ) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'Sta... Remove this comment to see the full error message
      smashServerUrl = this.environment.isSelfnode
        ? stakingConfig_1.SMASH_SERVERS_LIST.direct.url
        : stakingConfig_1.SMASH_SERVERS_LIST.iohk.url;
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.updateSmashSettingsRequest.execute(smashServerUrl);
    }
    (0, mobx_1.runInAction)(() => {
      this.smashServerUrl = smashServerUrl;
      this.smashServerLoading = false;
    });
  };
  _setSelectedDelegationWalletId = (walletId) => {
    this.selectedDelegationWalletId = walletId;
  };
  _sendStakePoolsSliderUsedAnalyticsEvent = (0, lodash_1.debounce)(() => {
    this.analytics.sendEvent(
      analytics_1.EventCategories.STAKE_POOLS,
      'Used stake pools amount slider'
    );
  }, 5000);
  _setStake = (stake) => {
    this.stake = stake;
    this._sendStakePoolsSliderUsedAnalyticsEvent();
  };
  _rankStakePools = () => {
    this.isRanking = true;
    this.getStakePoolsData();
  };
  _selectSmashServerUrl = async ({ smashServerUrl }) => {
    if (smashServerUrl && smashServerUrl !== this.smashServerUrl) {
      try {
        this.smashServerUrlError = null;
        // Retrieves the API update
        this.smashServerLoading = true;
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        await this.updateSmashSettingsRequest.execute(smashServerUrl);
        // Resets the Stake Pools list request
        this.stakePoolsRequest.reset();
        // Refreshes the Stake Pools list
        this.getStakePoolsData();
        // Starts the SPs fetch tracker
        this._startStakePoolsFetchTracker();
        // Updates the Smash Server URL
        (0, mobx_1.runInAction)(() => {
          this.smashServerUrl = smashServerUrl;
          this.smashServerUrlError = null;
          this.smashServerLoading = false;
        });
        // Update
        // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
        await this.api.localStorage.setSmashServer(smashServerUrl);
        this.analytics.sendEvent(
          analytics_1.EventCategories.SETTINGS,
          'Changed SMASH server',
          smashServerUrl
        );
      } catch (error) {
        (0, mobx_1.runInAction)(() => {
          this.smashServerUrlError = error;
          this.smashServerLoading = false;
        });
      }
    }
  };
  _startStakePoolsFetchTracker = () => {
    this._stopStakePoolsFetchTracker();
    this.isFetchingStakePools = true;
    this.stakePoolsFetchTrackerInterval = setInterval(
      this._stakePoolsFetchTracker,
      stakingConfig_1.STAKE_POOLS_FETCH_TRACKER_INTERVAL
    );
    this.getStakePoolsData(true);
  };
  _getStakingInfoWasOpen = async () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    const stakingInfoWasOpen = await this.api.localStorage.getStakingInfoWasOpen();
    (0, mobx_1.runInAction)(() => {
      this.stakingInfoWasOpen = stakingInfoWasOpen;
    });
  };
  _setStakingInfoWasOpen = () => {
    this.stakingInfoWasOpen = true;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'StakingStor... Remove this comment to see the full error message
    this.api.localStorage.setStakingInfoWasOpen();
  };
  _getStakePoolsListViewTooltip = async () => {
    const tooltipShown = await this.api.localStorage.getStakePoolsListViewTooltip();
    (0, mobx_1.runInAction)(() => {
      this.stakePoolsListViewTooltipVisible = tooltipShown;
    });
  };
  hideStakePoolsListViewTooltip = () => {
    this.stakePoolsListViewTooltipVisible = false;
    this.api.localStorage.setStakePoolsListViewTooltip(
      this.stakePoolsListViewTooltipVisible
    );
  };
  _stakePoolsFetchTracker = () => {
    const lastNumberOfStakePoolsFetched = this.numberOfStakePoolsFetched;
    this.numberOfStakePoolsFetched = this.stakePools.length;
    if (
      lastNumberOfStakePoolsFetched === this.numberOfStakePoolsFetched &&
      this.numberOfStakePoolsFetched > 0
    ) {
      this.cyclesWithoutIncreasingStakePools++;
    } else {
      this.cyclesWithoutIncreasingStakePools = 0;
    }
    if (
      this.cyclesWithoutIncreasingStakePools >=
      stakingConfig_1.STAKE_POOLS_FETCH_TRACKER_CYCLES
    ) {
      this._stopStakePoolsFetchTracker();
    }
  };
  _stopStakePoolsFetchTracker = () => {
    clearInterval(this.stakePoolsFetchTrackerInterval);
    this.numberOfStakePoolsFetched = 0;
    this.cyclesWithoutIncreasingStakePools = 0;
    this.isFetchingStakePools = false;
    this.getStakePoolsData();
  };
  _resetSmashServerError = () => {
    this.smashServerUrlError = null;
    this.smashServerLoading = false;
  };
  _joinStakePool = async (request) => {
    const { walletId, stakePoolId, passphrase, isHardwareWallet } = request;
    // Set join transaction in "PENDING" state
    this.isDelegationTransactionPending = true;
    try {
      let joinTransaction;
      if (isHardwareWallet) {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
        joinTransaction = await this.stores.hardwareWallets._sendMoney({
          isDelegationTransaction: true,
          selectedWalletId: walletId,
        });
      } else {
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        joinTransaction = await this.joinStakePoolRequest.execute({
          walletId,
          stakePoolId,
          passphrase,
        });
      }
      // Start interval to check transaction state every second
      this.delegationCheckTimeInterval = setInterval(
        this.checkDelegationTransaction,
        stakingConfig_1.STAKE_POOL_TRANSACTION_CHECK_INTERVAL,
        {
          transactionId: joinTransaction.id,
          walletId,
        }
      );
      // Reset transaction state check interval after 30 seconds
      setTimeout(() => {
        this.resetStakePoolTransactionChecker();
      }, stakingConfig_1.STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT);
      const wallet = this.stores.wallets.getWalletById(walletId);
      this.analytics.sendEvent(
        analytics_1.EventCategories.STAKE_POOLS,
        wallet.isDelegating ? 'Redelegated a wallet' : 'Delegated a wallet'
      );
    } catch (error) {
      this.resetStakePoolTransactionChecker();
      throw error;
    }
  };
  _quitStakePool = async (request) => {
    const { walletId, passphrase, isHardwareWallet } = request;
    // Set quit transaction in "PENDING" state
    this.isDelegationTransactionPending = true;
    try {
      let quitTransaction;
      if (isHardwareWallet) {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
        quitTransaction = await this.stores.hardwareWallets._sendMoney({
          isDelegationTransaction: true,
          selectedWalletId: walletId,
        });
      } else {
        // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
        quitTransaction = await this.quitStakePoolRequest.execute({
          walletId,
          passphrase,
        });
      }
      // Start interval to check transaction state every second
      this.delegationCheckTimeInterval = setInterval(
        this.checkDelegationTransaction,
        stakingConfig_1.STAKE_POOL_TRANSACTION_CHECK_INTERVAL,
        {
          transactionId: quitTransaction.id,
          walletId,
        }
      );
      // Reset transaction state check interval after 30 seconds
      setTimeout(() => {
        this.resetStakePoolTransactionChecker();
      }, stakingConfig_1.STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT);
    } catch (error) {
      this.resetStakePoolTransactionChecker();
      throw error;
    }
  };
  // Check stake pool transaction state and reset pending state when transction is "in_ledger"
  checkDelegationTransaction = (request) => {
    const { transactionId, walletId } = request;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const recentTransactionsResponse = this.stores.transactions._getTransactionsRecentRequest(
      walletId
    ).result;
    const recentTransactions = recentTransactionsResponse
      ? recentTransactionsResponse.transactions
      : [];
    // Return stake pool transaction when state is not "PENDING"
    const stakePoolTransaction = (0, lodash_1.find)(
      recentTransactions,
      (transaction) =>
        transaction.id === transactionId &&
        transaction.state === WalletTransaction_1.TransactionStates.OK
    );
    if (stakePoolTransaction) {
      this.resetStakePoolTransactionChecker();
    }
  };
  // Reset "PENDING" state, transaction state check poller and refresh wallets data
  resetStakePoolTransactionChecker = () => {
    if (this.delegationCheckTimeInterval) {
      clearInterval(this.delegationCheckTimeInterval);
      this.delegationCheckTimeInterval = null;
    }
    // this.stores.hardwareWallets._resetTransaction();
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    this.stores.wallets.refreshWalletsData();
    this.isDelegationTransactionPending = false;
  };
  _requestCSVFile = async ({ fileContent, filenamePrefix: prefix }) => {
    const {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Staking... Remove this comment to see the full error message
      actions: { wallets },
    } = this;
    const fileName = (0, files_1.generateFileNameWithTimestamp)({
      prefix,
      extension: 'csv',
      isUTC: true,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const { desktopDirectoryPath } = this.stores.profile;
    const defaultPath = path_1.default.join(desktopDirectoryPath, fileName);
    const params = {
      defaultPath,
      filters: [
        {
          extensions: ['csv'],
        },
      ],
    };
    const {
      filePath,
    } = await show_file_dialog_channels_1.showSaveDialogChannel.send(params);
    // if cancel button is clicked or path is empty
    if (!filePath) return;
    await wallets.generateCsv.trigger({
      fileContent,
      filePath,
    });
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Staking... Remove this comment to see the full error message
    this.actions.staking.requestCSVFileSuccess.trigger();
    this.analytics.sendEvent(
      analytics_1.EventCategories.STAKE_POOLS,
      'Exported rewards as CSV'
    );
  };
  calculateDelegationFee = async (delegationFeeRequest) => {
    const { walletId } = delegationFeeRequest;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const wallet = this.stores.wallets.getWalletById(walletId);
    this._delegationFeeCalculationWalletId = walletId;
    if (!wallet) {
      throw new Error(
        'Active wallet required before calculating transaction fees.'
      );
    }
    if (this.calculateDelegationFeeRequest.isExecuting) {
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.calculateDelegationFeeRequest;
    }
    try {
      const delegationFee = await this.calculateDelegationFeeRequest.execute({
        ...delegationFeeRequest,
      }).promise;
      if (this._delegationFeeCalculationWalletId !== walletId) {
        return null;
      }
      return delegationFee;
    } catch (error) {
      throw error;
    }
  };
  // GETTERS
  get currentRoute() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    return this.stores.router.location.pathname;
  }
  get isStakingPage() {
    return this.currentRoute.indexOf(routes_config_1.ROUTES.STAKING.ROOT) > -1;
  }
  get maxDelegationFunds() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const { desiredPoolNumber } = this.stores.networkStatus;
    return Math.round(stakingConfig_1.CIRCULATING_SUPPLY / desiredPoolNumber);
  }
  get stakePools() {
    return this.stakePoolsRequest.result ? this.stakePoolsRequest.result : [];
  }
  get recentStakePools() {
    const delegatedStakePools = [];
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    (0, lodash_1.map)(this.stores.wallets.all, (wallet) => {
      const hasPendingDelegations =
        wallet.pendingDelegations && wallet.pendingDelegations.length > 0;
      let lastDelegatedStakePoolId = wallet.delegatedStakePoolId;
      if (hasPendingDelegations) {
        lastDelegatedStakePoolId = wallet.lastDelegatedStakePoolId;
      }
      if (lastDelegatedStakePoolId) {
        const delegatingStakePoolExistInList = (0, lodash_1.find)(
          delegatedStakePools,
          (delegatedStakePool) =>
            delegatedStakePool.id === lastDelegatedStakePoolId
        );
        if (!delegatingStakePoolExistInList) {
          const delegatingStakePool = (0, lodash_1.find)(
            this.stakePools,
            (stakePool) => stakePool.id === lastDelegatedStakePoolId
          );
          if (delegatingStakePool)
            delegatedStakePools.push(delegatingStakePool);
        }
      }
    });
    const orderedStakePools = (0, lodash_1.orderBy)(
      delegatedStakePools,
      'ranking',
      'asc'
    );
    return orderedStakePools;
  }
  get isStakingDelegationCountdown() {
    return this.currentRoute === routes_config_1.ROUTES.STAKING.COUNTDOWN;
  }
  get rewards() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const { wallets } = this.stores;
    return wallets.allWallets.map((w) => this.getRewardForWallet(w));
  }
  getRewardForWallet(wallet) {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const { transactions, addresses } = this.stores;
    const rewardsAddress = addresses.stakeAddresses[wallet.id];
    const syncingProgress = wallet.syncState?.progress?.quantity;
    return {
      wallet: wallet.name,
      total: wallet.reward.plus(transactions.withdrawals[wallet.id]),
      unspent: wallet.reward,
      isRestoring: wallet.isRestoring,
      syncingProgress,
      rewardsAddress,
    };
  }
  showCountdown() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const { isShelleyPending } = this.stores.networkStatus;
    return isShelleyPending;
  }
  getStakePoolsData = async (isSmash) => {
    const {
      isConnected,
      isSynced,
      isShelleyActivated,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    } = this.stores.networkStatus;
    if (!isShelleyActivated || !isConnected || !isSynced) {
      this._resetIsRanking();
      return;
    }
    try {
      const stakeInBigNumber = new bignumber_js_1.default(this.stake);
      const stakeInLovelace = parseInt(
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BigNumber' is not assignable to ... Remove this comment to see the full error message
        stakeInBigNumber.times(numbersConfig_1.LOVELACES_PER_ADA),
        10
      );
      await this.stakePoolsRequest.execute(stakeInLovelace).promise;
      this._resetPolling(isSmash ? 'smash' : 'regular');
    } catch (error) {
      this._resetPolling('failed');
    }
    this._resetIsRanking();
  };
  _resetPolling = (type) => {
    if (type === 'kill') {
      this.fetchingStakePoolsFailed = true;
      if (this.pollingStakePoolsInterval) {
        clearInterval(this.pollingStakePoolsInterval);
        this.pollingStakePoolsInterval = null;
      }
      if (this.refreshPolling) {
        clearInterval(this.refreshPolling);
        this.refreshPolling = null;
      }
    } else if (type === 'failed') {
      this.fetchingStakePoolsFailed = true;
      if (this.pollingStakePoolsInterval) {
        clearInterval(this.pollingStakePoolsInterval);
        this.pollingStakePoolsInterval = null;
      }
      if (!this.refreshPolling) {
        this.refreshPolling = setInterval(
          this.getStakePoolsData,
          stakingConfig_1.STAKE_POOLS_FAST_INTERVAL
        );
      }
    } else {
      this.fetchingStakePoolsFailed = false;
      if (this.refreshPolling) {
        clearInterval(this.refreshPolling);
        this.refreshPolling = null;
      }
      clearInterval(this.pollingStakePoolsInterval);
      const isSmash = type === 'smash';
      const interval = isSmash
        ? stakingConfig_1.STAKE_POOLS_FETCH_TRACKER_INTERVAL
        : stakingConfig_1.STAKE_POOLS_INTERVAL;
      this.pollingStakePoolsInterval = setInterval(
        () => this.getStakePoolsData(isSmash),
        interval
      );
    }
  };
  _resetIsRanking = () => {
    this.isRanking = false;
  };
  // For testing only
  _setFakePoller = (forceLoading) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
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
        this._resetPolling('failed');
        return;
      }
      try {
        throw new Error('Faked "Stake pools" fetch error');
      } catch (error) {
        if (!this.refreshPolling) {
          this._resetPolling('failed');
        }
      }
    }
  };
  // For testing only
  _setFakedStakePools = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'Sta... Remove this comment to see the full error message
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
      configuration: stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIRMATION,
      confirmation: stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.RESULT,
      result: stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.RESULT,
    };
  }
  get prevStep() {
    return {
      configuration: stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIRMATION,
      confirmation: stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIGURATION,
      result: stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIGURATION,
    };
  }
  get redeemActions() {
    return {
      configuration: this._goToConfigurationStep,
      confirmation: this._goToConfirmationStep,
      result: this._goToResultStep,
    };
  }
  _goToConfigurationStep = () => {
    this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIGURATION;
  };
  _goToConfirmationStep = () => {
    this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIRMATION;
  };
  _goToResultStep = () => {
    this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.RESULT;
  };
  _onCalculateRedeemWalletFees = async ({ walletId, recoveryPhrase }) => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    this.redeemWallet = this.stores.wallets.getWalletById(walletId);
    this.redeemRecoveryPhrase = recoveryPhrase;
    this.isCalculatingReedemFees = true;
    try {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
      const [address] = await this.stores.addresses.getAddressesByWalletId(
        walletId
      );
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const transactionFees = await this.getRedeemItnRewardsFeeRequest.execute({
        wallet: this.redeemWallet,
        recoveryPhrase,
        address: address.id,
      });
      (0, mobx_1.runInAction)(() => {
        this.confirmationStepError = null;
        this.transactionFees = transactionFees;
        this.isCalculatingReedemFees = false;
      });
    } catch (error) {
      (0, mobx_1.runInAction)(() => {
        this.configurationStepError = error;
        this.transactionFees = null;
        this.isCalculatingReedemFees = false;
      });
    }
  };
  _onRedeemStart = () => {
    this.configurationStepError = null;
    this.confirmationStepError = null;
    this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIGURATION;
  };
  _onConfigurationContinue = () => {
    if (this.transactionFees && this.redeemRecoveryPhrase) {
      this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.CONFIRMATION;
      this.confirmationStepError = null;
      this.configurationStepError = null;
    } else {
      this.redeemSuccess = false;
      this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.RESULT;
    }
  };
  _onConfirmationContinue = async ({ spendingPassword }) => {
    const { redeemRecoveryPhrase: recoveryPhrase, redeemWallet } = this;
    this.isSubmittingReedem = true;
    if (!redeemWallet) throw new Error('Redeem wallet required');
    if (!recoveryPhrase) throw new Error('RecoveryPhrase required');
    const { id: walletId } = redeemWallet;
    try {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
      const [address] = await this.stores.addresses.getAddressesByWalletId(
        walletId
      );
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      const redeemedRewards = await this.requestRedeemItnRewardsRequest.execute(
        {
          address: address.id,
          walletId,
          spendingPassword,
          recoveryPhrase,
        }
      );
      (0, mobx_1.runInAction)(() => {
        this.redeemedRewards = redeemedRewards;
        this.redeemSuccess = true;
        this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.RESULT;
        this.confirmationStepError = null;
        this.isSubmittingReedem = false;
      });
    } catch (error) {
      (0, mobx_1.runInAction)(() => {
        this.confirmationStepError = error;
        this.isSubmittingReedem = false;
        if (error.id !== 'api.errors.IncorrectPasswordError') {
          this.redeemSuccess = false;
          this.redeemStep = stakingConfig_1.REDEEM_ITN_REWARDS_STEPS.RESULT;
        }
      });
    }
  };
  _onResultContinue = () => {
    if (!this.redeemWallet) throw new Error('Redeem wallet require');
    const { id } = this.redeemWallet;
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    this.stores.wallets.goToWalletRoute(id);
    this.redeemStep = null;
    this._resetRedeemItnRewards();
  };
  _resetRedeemItnRewards = () => {
    this.isSubmittingReedem = false;
    this.isCalculatingReedemFees = false;
    this.redeemSuccess = null;
    this.redeemWallet = null;
    this.transactionFees = null;
    this.redeemedRewards = null;
    this.redeemRecoveryPhrase = null;
    this.configurationStepError = null;
    this.confirmationStepError = null;
  };
  _closeRedeemDialog = () => {
    this._resetRedeemItnRewards();
    this.redeemStep = null;
  };
  // ================= REACTIONS ==================
  _pollOnSync = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'StakingS... Remove this comment to see the full error message
    const { isSynced, isShelleyActivated } = this.stores.networkStatus;
    if (isSynced && isShelleyActivated) {
      this.getStakePoolsData();
    } else {
      this._resetIsRanking();
      this._resetPolling('kill');
    }
  };
  /* ====  End of Redeem ITN Rewards  ===== */
  _goToStakingInfoPage = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Staking... Remove this comment to see the full error message
    this.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.STAKING.INFO,
    });
  };
  _goToStakingDelegationCenterPage = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Staking... Remove this comment to see the full error message
    this.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.STAKING.DELEGATION_CENTER,
    });
  };
  getStakePoolById = (stakePoolId) =>
    this.stakePools.find(({ id }) => id === stakePoolId);
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'isDelegationTransactionPending',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'fetchingStakePoolsFailed',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'selectedDelegationWalletId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'stake',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'isRanking',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  StakingStore.prototype,
  'smashServerUrl',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizableError_1.default)],
  StakingStore.prototype,
  'smashServerUrlError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'smashServerLoading',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'stakePoolsListViewTooltipVisible',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  StakingStore.prototype,
  'redeemStep',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  StakingStore.prototype,
  'redeemRecoveryPhrase',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Wallet_1.default)],
  StakingStore.prototype,
  'redeemWallet',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  StakingStore.prototype,
  'walletName',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  StakingStore.prototype,
  'transactionFees',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  StakingStore.prototype,
  'redeemedRewards',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'isSubmittingReedem',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'isCalculatingReedemFees',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  StakingStore.prototype,
  'redeemSuccess',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizableError_1.default)],
  StakingStore.prototype,
  'configurationStepError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizableError_1.default)],
  StakingStore.prototype,
  'confirmationStepError',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'isFetchingStakePools',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'numberOfStakePoolsFetched',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'cyclesWithoutIncreasingStakePools',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakingStore.prototype,
  'stakingInfoWasOpen',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'joinStakePoolRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'quitStakePoolRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'stakePoolsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'calculateDelegationFeeRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'getRedeemItnRewardsFeeRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'requestRedeemItnRewardsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'getSmashSettingsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  StakingStore.prototype,
  'updateSmashSettingsRequest',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_getSmashSettingsRequest',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_setSelectedDelegationWalletId',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_setStake',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_rankStakePools',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_selectSmashServerUrl',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_startStakePoolsFetchTracker',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_getStakingInfoWasOpen',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_setStakingInfoWasOpen',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_getStakePoolsListViewTooltip',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  'hideStakePoolsListViewTooltip',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_stakePoolsFetchTracker',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_stopStakePoolsFetchTracker',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_resetSmashServerError',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_joinStakePool',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_quitStakePool',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  'checkDelegationTransaction',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  'resetStakePoolTransactionChecker',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_requestCSVFile',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'currentRoute',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'isStakingPage',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'maxDelegationFunds',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'stakePools',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'recentStakePools',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'isStakingDelegationCountdown',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  StakingStore.prototype,
  'rewards',
  null
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', []),
    __metadata('design:returntype', Boolean),
  ],
  StakingStore.prototype,
  'showCountdown',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  'getStakePoolsData',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_resetPolling',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_resetIsRanking',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_setFakePoller',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_setFakedStakePools',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_goToConfigurationStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_goToConfirmationStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_goToResultStep',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_onCalculateRedeemWalletFees',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_onRedeemStart',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_onConfigurationContinue',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_onConfirmationContinue',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_onResultContinue',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_resetRedeemItnRewards',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  StakingStore.prototype,
  '_closeRedeemDialog',
  void 0
);
exports.default = StakingStore;
//# sourceMappingURL=StakingStore.js.map
