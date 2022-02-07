import { get, map } from 'lodash';
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

let LOCAL_TIME_DIFFERENCE = 0;
let SYNC_PROGRESS = null;
let TESTING_NEWSFEED_JSON: GetNewsResponse | null | undefined;
let TESTING_WALLETS_DATA: Record<string, any> = {};
export default (api: AdaApi) => {
  api.getNetworkInfo = async (): Promise<GetNetworkInfoResponse> => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.debug('AdaApi::getNetworkInfo (PATCHED) called');

    try {
      const networkInfo: NetworkInfoResponse = await getNetworkInfo(api.config);
      logger.debug('AdaApi::getNetworkInfo (PATCHED) success', {
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
        get(sync_progress, 'status') === 'ready'
          ? 100
          : get(sync_progress, 'quantity', 0);
      // extract relevant data before sending to NetworkStatusStore
      const nextEpochNumber = get(next_epoch, 'epoch_number', null);
      const nextEpochStartTime = get(next_epoch, 'epoch_start_time', '');
      return {
        syncProgress: SYNC_PROGRESS !== null ? SYNC_PROGRESS : syncProgress,
        localTip: {
          epoch: get(node_tip, 'epoch_number', 0),
          slot: get(node_tip, 'slot_number', 0),
          absoluteSlotNumber: get(node_tip, 'absolute_slot_number', 0),
        },
        networkTip: {
          epoch: get(network_tip, 'epoch_number', null),
          slot: get(network_tip, 'slot_number', null),
          absoluteSlotNumber: get(network_tip, 'absolute_slot_number', 0),
        },
        nextEpoch: {
          // N+1 epoch
          epochNumber: nextEpochNumber,
          epochStart: nextEpochStartTime,
        },
      };
    } catch (error) {
      logger.error('AdaApi::getNetworkInfo (PATCHED) error', {
        error,
      });
      throw new ApiError();
    }
  };

  api.setSyncProgress = async (syncProgress) => {
    SYNC_PROGRESS = syncProgress;
  };

  api.setTestingNewsFeed = (
    testingNewsFeedData: GetNewsResponse | null | undefined
  ) => {
    const { version: packageJsonVersion } = packageJson;

    if (!testingNewsFeedData) {
      TESTING_NEWSFEED_JSON = null;
      return;
    }

    // Always mutate newsfeed target version to current app version
    const newsFeedItems = map(testingNewsFeedData.items, (item) => {
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
    TESTING_NEWSFEED_JSON = { ...testingNewsFeedData, items: newsFeedItems };
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
    testingWalletData: Record<string, any>,
    walletIndex = 0
  ): void => {
    TESTING_WALLETS_DATA[walletIndex] = testingWalletData;
  };

  api.setTestingWallets = (
    testingWalletsData: Array<Record<string, any>>
  ): void => {
    TESTING_WALLETS_DATA = testingWalletsData;
  };

  const originalGetWallets: (...args: Array<any>) => any = api.getWallets;
  const getModifiedWallet = action((wallet: Record<string, any>) => {
    let { amount = 100000, availableAmount = 100000 } = wallet;
    if (typeof amount !== 'object') amount = new BigNumber(amount);
    if (typeof availableAmount !== 'object')
      availableAmount = new BigNumber(availableAmount);
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ amount: any; availableAmount: ... Remove this comment to see the full error message
    return new Wallet({ ...wallet, amount, availableAmount });
  });

  api.getWallets = async (): Promise<Array<Wallet>> => {
    const originalWallets = await originalGetWallets();
    const modifiedWallets = originalWallets.map(
      (originalWallet: Wallet, index: number) => {
        const testingWallet = TESTING_WALLETS_DATA[index] || {};
        const modifiedWallet = { ...originalWallet, ...testingWallet };
        return getModifiedWallet(modifiedWallet);
      }
    );
    return Promise.resolve(modifiedWallets);
  };

  api.setTestingStakePools = (
    testingStakePoolsData: Array<Record<string, any>>
  ): void => {
    api.getStakePools = (): Array<StakePool> =>
      // @ts-ignore ts-migrate(2739) FIXME: Type 'StakePool[]' is missing the following proper... Remove this comment to see the full error message
      testingStakePoolsData.map(
        // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'Record<string, any>' is not assi... Remove this comment to see the full error message
        (stakePool: Record<string, any>) => new StakePool(stakePool)
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
