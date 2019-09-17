// @flow
import { get } from 'lodash';
import AdaApi from '../api';
import { getNodeInfo } from '../nodes/requests/getNodeInfo';
import { getNodeSettings } from '../nodes/requests/getNodeSettings';
import { getLatestAppVersion } from '../nodes/requests/getLatestAppVersion';
import { GenericApiError } from '../common/errors';
import { Logger } from '../../utils/logging';
import type { NodeInfoQueryParams } from '../nodes/requests/getNodeInfo';
import type {
  LatestAppVersionInfoResponse,
  NodeInfoResponse,
  GetNetworkStatusResponse,
  NodeSettingsResponse,
  GetNodeSettingsResponse,
  GetLatestAppVersionResponse,
} from '../nodes/types';
import type { GetNewsResponse } from '../news/types';

let LATEST_APP_VERSION = null;
let LOCAL_TIME_DIFFERENCE = 0;
let LOCAL_BLOCK_HEIGHT = null;
let NETWORK_BLOCK_HEIGHT = null;
let NEXT_ADA_UPDATE = null;
let SUBSCRIPTION_STATUS = null;
let APPLICATION_VERSION = null;
let fakeNewsFeedJson: ?GetNewsResponse;

export default (api: AdaApi) => {
  api.getLocalTimeDifference = async () =>
    Promise.resolve(LOCAL_TIME_DIFFERENCE);

  api.getNetworkStatus = async (
    queryInfoParams?: NodeInfoQueryParams
  ): Promise<GetNetworkStatusResponse> => {
    Logger.debug('AdaApi::getNetworkStatus (PATCHED) called');
    try {
      const nodeInfo: NodeInfoResponse = await getNodeInfo(
        api.config,
        queryInfoParams
      );
      Logger.debug('AdaApi::getNetworkStatus (PATCHED) success', {
        nodeInfo,
      });

      const {
        blockchainHeight,
        subscriptionStatus,
        syncProgress,
        localBlockchainHeight,
      } = nodeInfo;

      // extract relevant data before sending to NetworkStatusStore
      const response = {
        subscriptionStatus: SUBSCRIPTION_STATUS || subscriptionStatus,
        syncProgress: syncProgress.quantity,
        blockchainHeight:
          NETWORK_BLOCK_HEIGHT || get(blockchainHeight, 'quantity', 0),
        localBlockchainHeight:
          LOCAL_BLOCK_HEIGHT || localBlockchainHeight.quantity,
        localTimeInformation: {
          status: 'available',
          difference: LOCAL_TIME_DIFFERENCE,
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
      Logger.error('AdaApi::getNetworkStatus (PATCHED) error', { error });
      throw new GenericApiError();
    }
  };

  api.getNodeSettings = async (): Promise<GetNodeSettingsResponse> => {
    Logger.debug('AdaApi::getNodeSettings (PATCHED) called');
    try {
      const nodeSettings: NodeSettingsResponse = await getNodeSettings(
        api.config
      );
      if (api.setFaultyNodeSettingsApi) {
        const error = new Error('getNodeSettings forced error');
        Logger.error('AdaApi::getNodeSettings (PATCHED) forced error', {
          error,
        });
        throw new GenericApiError(error);
      }
      Logger.debug('AdaApi::getNodeSettings (PATCHED) success', {
        nodeSettings,
      });
      const { slotId } = nodeSettings;
      return { slotId };
    } catch (error) {
      Logger.error('AdaApi::getNodeSettings (PATCHED) error', { error });
      throw new GenericApiError(error);
    }
  };

  api.setFaultyNodeSettingsApi = false;

  api.setLocalTimeDifference = async timeDifference => {
    LOCAL_TIME_DIFFERENCE = timeDifference;
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

  api.setSubscriptionStatus = async (subscriptionStatus: ?Object) => {
    SUBSCRIPTION_STATUS = subscriptionStatus;
  };

  api.setLocalBlockHeight = async (height: number) => {
    LOCAL_BLOCK_HEIGHT = height;
  };

  api.setNetworkBlockHeight = async (height: number) => {
    NETWORK_BLOCK_HEIGHT = height;
  };

  api.setFakeNewsFeedJsonForTesting = (json: ?GetNewsResponse) => {
    fakeNewsFeedJson = json;
  };

  api.getNews = (): Promise<GetNewsResponse> => {
    return new Promise((resolve, reject) => {
      if (!fakeNewsFeedJson) {
        reject(new Error('NEWS_FETCH_FAILED'));
      } else {
        resolve(fakeNewsFeedJson);
      }
    });
  };
};
