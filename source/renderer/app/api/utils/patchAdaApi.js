// @flow
import { get } from 'lodash';
import AdaApi from '../api';
import { getNetworkInfo } from '../network/requests/getNetworkInfo';
import { getLatestAppVersion } from '../nodes/requests/getLatestAppVersion';
import { GenericApiError } from '../common/errors';
import { Logger } from '../../utils/logging';
import type { NetworkInfoQueryParams } from '../network/requests/getNetworkInfo';
import type {
  GetNetworkInfoResponse,
  NetworkInfoResponse,
} from '../network/types';
import type {
  LatestAppVersionInfoResponse,
  GetLatestAppVersionResponse,
} from '../nodes/types';
import type { GetNewsResponse } from '../news/types';

let LATEST_APP_VERSION = null;
let LOCAL_TIME_DIFFERENCE = 0;
let SYNC_PROGRESS = null;
let NEXT_ADA_UPDATE = null;
let APPLICATION_VERSION = null;
let FAKE_NEWSFEED_JSON: ?GetNewsResponse;

export default (api: AdaApi) => {
  api.getLocalTimeDifference = async () =>
    Promise.resolve(LOCAL_TIME_DIFFERENCE);

  api.getNetworkInfo = async (
    queryInfoParams?: NetworkInfoQueryParams
  ): Promise<GetNetworkInfoResponse> => {
    Logger.debug('AdaApi::getNetworkInfo (PATCHED) called');
    try {
      const networkInfo: NetworkInfoResponse = await getNetworkInfo(
        api.config,
        queryInfoParams
      );
      Logger.debug('AdaApi::getNetworkInfo (PATCHED) success', {
        networkInfo,
      });

      /* eslint-disable-next-line camelcase */
      const { sync_progress, node_tip, network_tip } = networkInfo;
      const syncProgress =
        get(sync_progress, 'status') === 'ready'
          ? 100
          : get(sync_progress, 'quantity', 0);

      // extract relevant data before sending to NetworkStatusStore
      const response = {
        syncProgress: SYNC_PROGRESS || syncProgress,
        localTip: {
          epoch: get(node_tip, 'epoch_number', 0),
          slot: get(node_tip, 'slot_number', 0),
        },
        networkTip: {
          epoch: get(network_tip, 'epoch_number', 0),
          slot: get(network_tip, 'slot_number', 0),
        },
        localTimeInformation: {
          status: 'available',
          difference: 0 || LOCAL_TIME_DIFFERENCE,
        },
      };

      // Since in test environment we run multiple NTP force-checks
      // we need to protect ourselves from getting punished by the NTP
      // service which results in 30 second delay in NTP check response.
      // In order to simulate NTP force-check we use 250ms timeout.
      const isForcedTimeDifferenceCheck = !!queryInfoParams;
      return isForcedTimeDifferenceCheck
        ? new Promise(resolve => {
            setTimeout(() => resolve(response), 250);
          })
        : response;
    } catch (error) {
      Logger.error('AdaApi::getNetworkInfo (PATCHED) error', { error });
      throw new GenericApiError();
    }
  };

  api.setLocalTimeDifference = async timeDifference => {
    LOCAL_TIME_DIFFERENCE = timeDifference;
  };

  api.setSyncProgress = async syncProgress => {
    SYNC_PROGRESS = syncProgress;
  };

  api.nextUpdate = async () => {
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

  api.setFakeNewsFeedJsonForTesting = (fakeNewsfeedJson: ?GetNewsResponse) => {
    FAKE_NEWSFEED_JSON = fakeNewsfeedJson;
  };

  api.getNews = (): Promise<GetNewsResponse> => {
    return new Promise((resolve, reject) => {
      if (!FAKE_NEWSFEED_JSON) {
        reject(new Error('Unable to fetch news'));
      } else {
        resolve(FAKE_NEWSFEED_JSON);
      }
    });
  };

  api.resetTestOverrides = () => {
    LATEST_APP_VERSION = null;
    LOCAL_TIME_DIFFERENCE = 0;
    NEXT_ADA_UPDATE = null;
    APPLICATION_VERSION = null;
  };
};
