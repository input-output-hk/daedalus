// @flow
import { get, map } from 'lodash';
import moment from 'moment';
import { action } from 'mobx';
import BigNumber from 'bignumber.js/bignumber';
import AdaApi from '../api';
import { getNetworkInfo } from '../network/requests/getNetworkInfo';
import { logger } from '../../utils/logging';
import packageJson from '../../../../../package.json';
import ApiError from '../../domains/ApiError';

// domains
import Wallet from '../../domains/Wallet';
import StakePool from '../../domains/StakePool';

import type {
  GetNetworkInfoResponse,
  NetworkInfoResponse,
} from '../network/types';
import type { GetNewsResponse } from '../news/types';
import { getEpochLength } from '../../config/epochsConfig';

let LOCAL_TIME_DIFFERENCE = 0;
let SYNC_PROGRESS = null;
let TESTING_NEWSFEED_JSON: ?GetNewsResponse;
let TESTING_WALLETS_DATA: Object = {};

export default (api: AdaApi) => {
  api.getNetworkInfo = async (): Promise<GetNetworkInfoResponse> => {
    logger.debug('AdaApi::getNetworkInfo (PATCHED) called');
    try {
      const networkInfo: NetworkInfoResponse = await getNetworkInfo(api.config);
      logger.debug('AdaApi::getNetworkInfo (PATCHED) success', { networkInfo });

      const {
        sync_progress, // eslint-disable-line camelcase
        node_tip, // eslint-disable-line camelcase
        network_tip, // eslint-disable-line camelcase
        next_epoch, // eslint-disable-line camelcase
      } = networkInfo;
      const syncProgress =
        get(sync_progress, 'status') === 'ready'
          ? 100
          : get(sync_progress, 'quantity', 0);

      // extract relevant data before sending to NetworkStatusStore
      const nextEpochNumber = get(next_epoch, 'epoch_number', null);
      const nextEpochStartTime = get(next_epoch, 'epoch_start_time', '');
      const epochLength = getEpochLength();
      return {
        syncProgress: SYNC_PROGRESS !== null ? SYNC_PROGRESS : syncProgress,
        localTip: {
          epoch: get(node_tip, 'epoch_number', 0),
          slot: get(node_tip, 'slot_number', 0),
        },
        networkTip: {
          epoch: get(network_tip, 'epoch_number', null),
          slot: get(network_tip, 'slot_number', null),
        },
        nextEpoch: {
          // N+1 epoch
          epochNumber: nextEpochNumber,
          epochStart: nextEpochStartTime,
        },
        futureEpoch: {
          // N+2 epoch
          epochNumber: nextEpochNumber ? nextEpochNumber + 1 : null,
          epochStart: nextEpochStartTime
            ? moment(nextEpochStartTime)
                .add(epochLength, 'seconds')
                .toISOString()
            : '',
        },
      };
    } catch (error) {
      logger.error('AdaApi::getNetworkInfo (PATCHED) error', { error });
      throw new ApiError();
    }
  };

  api.setSyncProgress = async syncProgress => {
    SYNC_PROGRESS = syncProgress;
  };

  api.setTestingNewsFeed = (testingNewsFeedData: ?GetNewsResponse) => {
    const { version: packageJsonVersion } = packageJson;
    if (!testingNewsFeedData) {
      TESTING_NEWSFEED_JSON = null;
      return;
    }
    // Always mutate newsfeed target version to current app version
    const newsFeedItems = map(testingNewsFeedData.items, item => {
      return {
        ...item,
        target: {
          ...item.target,
          daedalusVersion: item.target.daedalusVersion
            ? packageJsonVersion
            : '',
        },
      };
    });

    TESTING_NEWSFEED_JSON = {
      ...testingNewsFeedData,
      items: newsFeedItems,
    };
  };

  api.getNews = (): Promise<GetNewsResponse> => {
    return new Promise((resolve, reject) => {
      if (!TESTING_NEWSFEED_JSON) {
        reject(new Error('Unable to fetch news'));
      } else {
        resolve(TESTING_NEWSFEED_JSON);
      }
    });
  };

  api.setTestingWallet = (
    testingWalletData: Object,
    walletIndex?: number = 0
  ): void => {
    TESTING_WALLETS_DATA[walletIndex] = testingWalletData;
  };

  api.setTestingWallets = (testingWalletsData: Array<Object>): void => {
    TESTING_WALLETS_DATA = testingWalletsData;
  };

  const originalGetWallets: Function = api.getWallets;

  const getModifiedWallet = action((wallet: Object) => {
    let { amount = 100000, availableAmount = 100000 } = wallet;
    if (typeof amount !== 'object') amount = new BigNumber(amount);
    if (typeof availableAmount !== 'object')
      availableAmount = new BigNumber(availableAmount);
    return new Wallet({
      ...wallet,
      amount,
      availableAmount,
    });
  });

  api.getWallets = async (request: {
    isShelleyActivated: boolean,
  }): Promise<Array<Wallet>> => {
    const { isShelleyActivated } = request;
    const originalWallets = await originalGetWallets({ isShelleyActivated });
    const modifiedWallets = originalWallets.map(
      (originalWallet: Wallet, index: number) => {
        const testingWallet = TESTING_WALLETS_DATA[index] || {};
        const modifiedWallet = {
          ...originalWallet,
          ...testingWallet,
        };
        return getModifiedWallet(modifiedWallet);
      }
    );
    return Promise.resolve(modifiedWallets);
  };

  api.setTestingStakePools = (testingStakePoolsData: Array<Object>): void => {
    api.getStakePools = (): Array<StakePool> =>
      testingStakePoolsData.map(
        (stakePool: Object) => new StakePool(stakePool)
      );
  };

  api.setLocalTimeDifference = async timeDifference => {
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
    LATEST_APP_VERSION = null;
    APPLICATION_VERSION = null;
    LOCAL_TIME_DIFFERENCE = 0;
  };
};
