// @flow
import { get, map } from 'lodash';
import moment from 'moment';
import { action } from 'mobx';
import BigNumber from 'bignumber.js/bignumber';
import AdaApi from '../api';
import { getNetworkInfo } from '../network/requests/getNetworkInfo';
import { getLatestAppVersion } from '../nodes/requests/getLatestAppVersion';
import { GenericApiError } from '../common/errors';
import { Logger } from '../../utils/logging';
import packageJson from '../../../../../package.json';

// domains
import Wallet from '../../domains/Wallet';
import StakePool from '../../domains/StakePool';

import type {
  GetNetworkInfoResponse,
  NetworkInfoResponse,
} from '../network/types';
import type {
  LatestAppVersionInfoResponse,
  GetLatestAppVersionResponse,
} from '../nodes/types';
import type { GetNewsResponse } from '../news/types';
import { EPOCH_LENGTH_ITN } from '../../config/epochsConfig';

let LATEST_APP_VERSION = null;
let SYNC_PROGRESS = null;
let NEXT_ADA_UPDATE = null;
let APPLICATION_VERSION = null;
let TESTING_NEWSFEED_JSON: ?GetNewsResponse;
let TESTING_WALLETS_DATA: Object = {};

export default (api: AdaApi) => {
  api.getNetworkInfo = async (): Promise<GetNetworkInfoResponse> => {
    Logger.debug('AdaApi::getNetworkInfo (PATCHED) called');
    try {
      const networkInfo: NetworkInfoResponse = await getNetworkInfo(api.config);
      Logger.debug('AdaApi::getNetworkInfo (PATCHED) success', { networkInfo });

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
      return {
        syncProgress: SYNC_PROGRESS || syncProgress,
        localTip: {
          epoch: get(node_tip, 'epoch_number', 0),
          slot: get(node_tip, 'slot_number', 0),
        },
        networkTip: {
          epoch: get(network_tip, 'epoch_number', 0),
          slot: get(network_tip, 'slot_number', 0),
        },
        nextEpoch: {
          epochNumber: get(next_epoch, 'epoch_number', 0),
          epochStart: get(next_epoch, 'epoch_start_time', ''),
        },
        futureEpoch: {
          epochNumber: get(next_epoch, 'epoch_number', 0) + 1,
          epochStart: moment(get(next_epoch, 'epoch_start', '')).add(
            EPOCH_LENGTH_ITN,
            'seconds'
          ),
        },
      };
    } catch (error) {
      Logger.error('AdaApi::getNetworkInfo (PATCHED) error', { error });
      throw new GenericApiError();
    }
  };

  api.setSyncProgress = async syncProgress => {
    SYNC_PROGRESS = syncProgress;
  };

  api.nextUpdate = async (): Promise<Object> => {
    let nodeUpdate = null;

    if (NEXT_ADA_UPDATE) {
      nodeUpdate = {
        version: NEXT_ADA_UPDATE,
      };
    }

    Logger.debug('AdaApi::nextUpdate success', { nodeUpdate });
    return Promise.resolve(nodeUpdate);
  };

  api.setNextUpdate = async nextUpdate => {
    NEXT_ADA_UPDATE = nextUpdate;
  };

  api.getLatestAppVersion = async (): Promise<GetLatestAppVersionResponse> => {
    Logger.debug('AdaApi::getLatestAppVersion (PATCHED) called');
    try {
      const { isWindows, platform } = global.environment;
      const latestAppVersionInfo: LatestAppVersionInfoResponse = await getLatestAppVersion();
      const latestAppVersionPath = `platforms.${
        isWindows ? 'windows' : platform
      }.version`;

      const applicationVersionPath = `platforms.${
        isWindows ? 'windows' : platform
      }.applicationVersion`;

      const latestAppVersion = get(
        latestAppVersionInfo,
        latestAppVersionPath,
        null
      );

      const applicationVersion = get(
        latestAppVersionInfo,
        applicationVersionPath,
        null
      );

      Logger.debug('AdaApi::getLatestAppVersion success', {
        latestAppVersion,
        latestAppVersionInfo,
        applicationVersion,
      });

      return {
        latestAppVersion: LATEST_APP_VERSION || latestAppVersion,
        applicationVersion: APPLICATION_VERSION || applicationVersion,
      };
    } catch (error) {
      Logger.error('AdaApi::getLatestAppVersion (PATCHED) error', { error });
      throw new GenericApiError();
    }
  };

  api.setLatestAppVersion = async (latestAppVersion: ?string) => {
    LATEST_APP_VERSION = latestAppVersion;
  };

  api.setApplicationVersion = async (applicationVersion: number) => {
    APPLICATION_VERSION = applicationVersion;
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

  api.getWallets = async (): Promise<Array<Wallet>> => {
    const originalWallets = await originalGetWallets();
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

  api.resetTestOverrides = () => {
    TESTING_WALLETS_DATA = {};
    LATEST_APP_VERSION = null;
    NEXT_ADA_UPDATE = null;
    APPLICATION_VERSION = null;
  };
};
