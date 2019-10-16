// @flow
import BigNumber from 'bignumber.js';
import { get } from 'lodash';
import AdaApi from '../api';
import { getNodeInfo } from '../nodes/requests/getNodeInfo';
import { getNodeSettings } from '../nodes/requests/getNodeSettings';
import { GenericApiError } from '../common/errors';
import { Logger } from '../../utils/logging';
import { RedeemAdaError } from '../transactions/errors';
import type { RedeemAdaParams } from '../transactions/requests/redeemAda';
import type { RedeemPaperVendedAdaParams } from '../transactions/requests/redeemPaperVendedAda';
import type { NodeInfoQueryParams } from '../nodes/requests/getNodeInfo';
import type {
  NodeInfoResponse,
  GetNetworkStatusResponse,
  NodeSettingsResponse,
  GetNodeSettingsResponse,
} from '../nodes/types';

let LOCAL_TIME_DIFFERENCE = 0;
let LOCAL_BLOCK_HEIGHT = null;
let NETWORK_BLOCK_HEIGHT = null;
let SUBSCRIPTION_STATUS = null;

export default (api: AdaApi) => {
  // Since we cannot test ada redemption in dev mode, just resolve the requests
  api.redeemAda = (request: RedeemAdaParams): Promise<any> =>
    new Promise(resolve => {
      try {
        Logger.debug('AdaApi::redeemAda (PATCHED) called', { request });
        const { redemptionCode } = request;
        const isValidRedemptionCode = api.isValidRedemptionKey(redemptionCode);
        if (!isValidRedemptionCode) {
          Logger.debug(
            'AdaApi::redeemAda (PATCHED) failed: not a valid redemption key!'
          );
          throw new RedeemAdaError();
        }
        Logger.debug('AdaApi::redeemAda (PATCHED) success');
        resolve({ amount: new BigNumber(1000) });
      } catch (error) {
        Logger.error('AdaApi::redeemAda (PATCHED) error', { error });
        throw new RedeemAdaError();
      }
    });

  api.redeemPaperVendedAda = (
    request: RedeemPaperVendedAdaParams
  ): Promise<any> =>
    new Promise(resolve => {
      try {
        Logger.debug('AdaApi::redeemPaperVendedAda (PATCHED) called', {
          request,
        });
        const { redemptionCode, mnemonic } = request;
        const isValidKey = api.isValidPaperVendRedemptionKey(redemptionCode);
        const isValidMnemonic = api.isValidRedemptionMnemonic(
          mnemonic.join(' ')
        );
        if (!isValidKey)
          Logger.debug(
            'AdaApi::redeemPaperVendedAda (PATCHED) failed: not a valid redemption key!'
          );
        if (!isValidMnemonic)
          Logger.debug(
            'AdaApi::redeemPaperVendedAda (PATCHED) failed: not a valid mnemonic!'
          );
        if (!isValidKey || !isValidMnemonic) {
          throw new RedeemAdaError();
        }
        Logger.debug('AdaApi::redeemPaperVendedAda (PATCHED) success');
        resolve({ amount: new BigNumber(1000) });
      } catch (error) {
        Logger.error('AdaApi::redeemPaperVendedAda (PATCHED) error', { error });
        throw new RedeemAdaError();
      }
    });

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

  api.setSubscriptionStatus = async (subscriptionStatus: ?Object) => {
    SUBSCRIPTION_STATUS = subscriptionStatus;
  };

  api.setLocalBlockHeight = async (height: number) => {
    LOCAL_BLOCK_HEIGHT = height;
  };

  api.setNetworkBlockHeight = async (height: number) => {
    NETWORK_BLOCK_HEIGHT = height;
  };
};
