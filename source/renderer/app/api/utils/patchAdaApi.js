'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const lodash_1 = require('lodash');
const mobx_1 = require('mobx');
const bignumber_1 = __importDefault(require('bignumber.js/bignumber'));
const getNetworkInfo_1 = require('../network/requests/getNetworkInfo');
const logging_1 = require('../../utils/logging');
const package_json_1 = __importDefault(require('../../../../../package.json'));
const ApiError_1 = __importDefault(require('../../domains/ApiError'));
// domains
const Wallet_1 = __importDefault(require('../../domains/Wallet'));
const StakePool_1 = __importDefault(require('../../domains/StakePool'));
let LOCAL_TIME_DIFFERENCE = 0;
let SYNC_PROGRESS = null;
let TESTING_NEWSFEED_JSON;
let TESTING_WALLETS_DATA = {};
exports.default = (api) => {
  api.getNetworkInfo = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.debug('AdaApi::getNetworkInfo (PATCHED) called');
    try {
      const networkInfo = await (0, getNetworkInfo_1.getNetworkInfo)(
        api.config
      );
      logging_1.logger.debug('AdaApi::getNetworkInfo (PATCHED) success', {
        networkInfo,
      });
      const {
        sync_progress,
        // eslint-disable-line camelcase
        node_tip,
        // eslint-disable-line camelcase
        network_tip,
        // eslint-disable-line camelcase
        next_epoch, // eslint-disable-line camelcase
      } = networkInfo;
      const syncProgress =
        (0, lodash_1.get)(sync_progress, 'status') === 'ready'
          ? 100
          : (0, lodash_1.get)(sync_progress, 'quantity', 0);
      // extract relevant data before sending to NetworkStatusStore
      const nextEpochNumber = (0, lodash_1.get)(
        next_epoch,
        'epoch_number',
        null
      );
      const nextEpochStartTime = (0, lodash_1.get)(
        next_epoch,
        'epoch_start_time',
        ''
      );
      return {
        syncProgress: SYNC_PROGRESS !== null ? SYNC_PROGRESS : syncProgress,
        localTip: {
          epoch: (0, lodash_1.get)(node_tip, 'epoch_number', 0),
          slot: (0, lodash_1.get)(node_tip, 'slot_number', 0),
          absoluteSlotNumber: (0, lodash_1.get)(
            node_tip,
            'absolute_slot_number',
            0
          ),
        },
        networkTip: {
          epoch: (0, lodash_1.get)(network_tip, 'epoch_number', null),
          slot: (0, lodash_1.get)(network_tip, 'slot_number', null),
          absoluteSlotNumber: (0, lodash_1.get)(
            network_tip,
            'absolute_slot_number',
            0
          ),
        },
        nextEpoch: {
          // N+1 epoch
          epochNumber: nextEpochNumber,
          epochStart: nextEpochStartTime,
        },
      };
    } catch (error) {
      logging_1.logger.error('AdaApi::getNetworkInfo (PATCHED) error', {
        error,
      });
      throw new ApiError_1.default();
    }
  };
  api.setSyncProgress = async (syncProgress) => {
    SYNC_PROGRESS = syncProgress;
  };
  api.setTestingNewsFeed = (testingNewsFeedData) => {
    const { version: packageJsonVersion } = package_json_1.default;
    if (!testingNewsFeedData) {
      TESTING_NEWSFEED_JSON = null;
      return;
    }
    // Always mutate newsfeed target version to current app version
    const newsFeedItems = (0, lodash_1.map)(
      testingNewsFeedData.items,
      (item) => {
        return {
          ...item,
          target: {
            ...item.target,
            daedalusVersion: item.target.daedalusVersion
              ? packageJsonVersion
              : '',
          },
        };
      }
    );
    TESTING_NEWSFEED_JSON = { ...testingNewsFeedData, items: newsFeedItems };
  };
  api.getNews = () => {
    return new Promise((resolve, reject) => {
      if (!TESTING_NEWSFEED_JSON) {
        reject(new Error('Unable to fetch news'));
      } else {
        resolve(TESTING_NEWSFEED_JSON);
      }
    });
  };
  api.setTestingWallet = (testingWalletData, walletIndex = 0) => {
    TESTING_WALLETS_DATA[walletIndex] = testingWalletData;
  };
  api.setTestingWallets = (testingWalletsData) => {
    TESTING_WALLETS_DATA = testingWalletsData;
  };
  const originalGetWallets = api.getWallets;
  const getModifiedWallet = (0, mobx_1.action)((wallet) => {
    let { amount = 100000, availableAmount = 100000 } = wallet;
    if (typeof amount !== 'object') amount = new bignumber_1.default(amount);
    if (typeof availableAmount !== 'object')
      availableAmount = new bignumber_1.default(availableAmount);
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ amount: any; availableAmount: ... Remove this comment to see the full error message
    return new Wallet_1.default({ ...wallet, amount, availableAmount });
  });
  api.getWallets = async () => {
    const originalWallets = await originalGetWallets();
    const modifiedWallets = originalWallets.map((originalWallet, index) => {
      const testingWallet = TESTING_WALLETS_DATA[index] || {};
      const modifiedWallet = { ...originalWallet, ...testingWallet };
      return getModifiedWallet(modifiedWallet);
    });
    return Promise.resolve(modifiedWallets);
  };
  api.setTestingStakePools = (testingStakePoolsData) => {
    api.getStakePools = () =>
      // @ts-ignore ts-migrate(2739) FIXME: Type 'StakePool[]' is missing the following proper... Remove this comment to see the full error message
      testingStakePoolsData.map(
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Record<string, any>' is not assi... Remove this comment to see the full error message
        (stakePool) => new StakePool_1.default(stakePool)
      );
  };
  api.setLocalTimeDifference = async (timeDifference) => {
    LOCAL_TIME_DIFFERENCE = timeDifference;
  };
  api.getNetworkClock = async () => {
    return {
      status: 'available',
      offset: LOCAL_TIME_DIFFERENCE,
    };
  };
  api.resetTestOverrides = () => {
    TESTING_WALLETS_DATA = {};
    SYNC_PROGRESS = null;
    LOCAL_TIME_DIFFERENCE = 0;
  };
};
//# sourceMappingURL=patchAdaApi.js.map
