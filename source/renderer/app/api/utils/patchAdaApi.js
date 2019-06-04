// @flow
import BigNumber from 'bignumber.js';
import { get } from 'lodash';
import AdaApi from '../api';
import { getNodeInfo } from '../nodes/requests/getNodeInfo';
import { GenericApiError } from '../common/errors';
import { Logger } from '../../utils/logging';
import { RedeemAdaError } from '../transactions/errors';
import type { RedeemAdaParams } from '../transactions/requests/redeemAda';
import type { RedeemPaperVendedAdaParams } from '../transactions/requests/redeemPaperVendedAda';
import type { NodeQueryParams } from '../nodes/requests/getNodeInfo';
import type { NodeInfo, GetNetworkStatusResponse } from '../nodes/types';

// ========== LOGGING =========

let LOCAL_TIME_DIFFERENCE = 0;
let NEXT_ADA_UPDATE = null;
let NODE_SUBSCRIPTION_STATUS = null;

export default (api: AdaApi) => {
  // Since we cannot test ada redemption in dev mode, just resolve the requests
  api.redeemAda = (
    request: RedeemAdaParams
  ): Promise<any> => new Promise((resolve) => {
    try {
      Logger.debug('AdaApi::redeemAda (PATCHED) called', { request });
      const { redemptionCode } = request;
      const isValidRedemptionCode = api.isValidRedemptionKey(redemptionCode);
      if (!isValidRedemptionCode) {
        Logger.debug('AdaApi::redeemAda (PATCHED) failed: not a valid redemption key!');
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
  ): Promise<any> => new Promise((resolve) => {
    try {
      Logger.debug('AdaApi::redeemPaperVendedAda (PATCHED) called', { request });
      const { redemptionCode, mnemonic } = request;
      const isValidKey = api.isValidPaperVendRedemptionKey(redemptionCode);
      const isValidMnemonic = api.isValidRedemptionMnemonic(mnemonic.join(' '));
      if (!isValidKey) Logger.debug('AdaApi::redeemPaperVendedAda (PATCHED) failed: not a valid redemption key!');
      if (!isValidMnemonic) Logger.debug('AdaApi::redeemPaperVendedAda (PATCHED) failed: not a valid mnemonic!');
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

  api.getLocalTimeDifference = async () => (
    Promise.resolve(LOCAL_TIME_DIFFERENCE)
  );

  api.getNetworkStatus = async (
    queryParams?: NodeQueryParams
  ): Promise<GetNetworkStatusResponse> => {
    Logger.debug('AdaApi::getNetworkStatus (PATCHED) called');
    try {
      const status: NodeInfo = await getNodeInfo(api.config, queryParams);
      Logger.debug('AdaApi::getNetworkStatus (PATCHED) success', { status });

      const {
        blockchainHeight,
        subscriptionStatus,
        syncProgress,
        localBlockchainHeight,
      } = status;

      // extract relevant data before sending to NetworkStatusStore
      return {
        subscriptionStatus: NODE_SUBSCRIPTION_STATUS || subscriptionStatus,
        syncProgress: syncProgress.quantity,
        blockchainHeight: get(blockchainHeight, 'quantity', 0),
        localBlockchainHeight: localBlockchainHeight.quantity,
        localTimeInformation: {
          status: 'available',
          difference: LOCAL_TIME_DIFFERENCE,
        },
      };
    } catch (error) {
      Logger.error('AdaApi::getNetworkStatus (PATCHED) error', { error });
      throw new GenericApiError();
    }
  };

  api.setLocalTimeDifference = async (timeDifference) => {
    LOCAL_TIME_DIFFERENCE = timeDifference;
  };

  api.nextUpdate = async () => (
    Promise.resolve(NEXT_ADA_UPDATE)
  );

  api.setNextUpdate = async (nextUpdate) => {
    NEXT_ADA_UPDATE = nextUpdate;
  };

  api.unsubscribeNode = async () => {
    NODE_SUBSCRIPTION_STATUS = {};
  };

  api.getLatestAppVersionInfo = async () => ({
    platforms: {
      windows: {
        version: '0.14.0',
      },
      darwin: {
        version: '0.14.0',
      },
      linux: {
        version: '0.14.0',
      },
    }
  });
};
