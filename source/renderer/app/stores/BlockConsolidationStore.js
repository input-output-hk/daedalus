// @flow
import { observable, action, runInAction } from 'mobx';

import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  BLOCK_CONSOLIDATION_IPC_REQUEST_INTERVAL,
  BLOCK_CONSOLIDATION_API_REQUEST_INTERVAL,
} from '../config/timingConfig';
import { Logger } from '../utils/logging';
import { getNumberOfEpochsConsolidatedChannel } from '../ipc/getNumberOfEpochsConsolidatedChannel';
import type { GetConsolidatedEpochsCountMainResponse } from '../../../common/ipc/api';
import type {
  GetNodeSettingsResponse,
  GetCurrentEpochFallbackResponse,
} from '../api/nodes/types';

export default class BlockConsolidationStore extends Store {
  // Initialize store observables
  @observable epochsConsolidated: number = 0; // Got from the IPC channel
  @observable currentEpoch: number = 0; // Got from the API (or from cardanoexplorer when the API fails)

  @observable
  getNodeSettingsRequest: Request<GetNodeSettingsResponse> = new Request(
    this.api.ada.getNodeSettings
  );
  @observable
  getCurrentEpochFallbackRequest: Request<GetCurrentEpochFallbackResponse> = new Request(
    this.api.ada.getCurrentEpochFallback
  );

  pollingIpcInterval: ?IntervalID = null;
  pollingApiInterval: ?IntervalID = null;

  setup() {
    const actions = this.actions.blockConsolidation;
    actions.stopBlockConsolidationDataFetch.listen(
      this._stopBlockConsolidationDataFetch
    );
    actions.startBlockConsolidationDataFetch.listen(
      this._startBlockConsolidationDataFetch
    );
  }

  _startBlockConsolidationDataFetch = () => {
    this._getBlockConsolidationIpcData();
    this._getBlockConsolidationApiData();
    this._stopBlockConsolidationDataFetch();

    this.pollingIpcInterval = setInterval(
      this._getBlockConsolidationIpcData,
      BLOCK_CONSOLIDATION_IPC_REQUEST_INTERVAL
    );
    this.pollingApiInterval = setInterval(
      this._getBlockConsolidationApiData,
      BLOCK_CONSOLIDATION_API_REQUEST_INTERVAL
    );
  };

  _stopBlockConsolidationDataFetch = () => {
    if (this.pollingIpcInterval) clearInterval(this.pollingIpcInterval);
    if (this.pollingApiInterval) clearInterval(this.pollingApiInterval);
  };

  // DEFINE ACTIONS

  @action _getBlockConsolidationIpcData = async () => {
    Logger.debug(
      'BlockConsolidationStore: _getBlockConsolidationIpcData called'
    );
    const epochsConsolidated: GetConsolidatedEpochsCountMainResponse = await getNumberOfEpochsConsolidatedChannel.request();
    try {
      runInAction('current epoch', () => {
        this.epochsConsolidated = epochsConsolidated;
      });
    } catch (error) {
      Logger.error(
        'BlockConsolidationStore: _getBlockConsolidationIpcData failed',
        {
          error,
        }
      );
    }
  };

  @action _getBlockConsolidationApiData = async () => {
    try {
      Logger.debug(
        'BlockConsolidationStore: _getBlockConsolidationApiData called'
      );
      const nodeSettings: GetNodeSettingsResponse = await this.getNodeSettingsRequest.execute()
        .promise;
      const { slotId } = nodeSettings;
      if (slotId && slotId.epoch) {
        runInAction('current epoch', () => {
          this.currentEpoch = slotId.epoch;
        });
      } else {
        Logger.error(
          'BlockConsolidationStore: _getBlockConsolidationApiData failed',
          {
            error: 'API did not return `slotId`',
          }
        );
        this._getCurrentEpochFallback();
      }
    } catch (error) {
      Logger.error(
        'BlockConsolidationStore: _getBlockConsolidationApiData failed',
        {
          error,
        }
      );
      this._getCurrentEpochFallback();
    }
  };

  @action _getCurrentEpochFallback = async () => {
    if (this.pollingApiInterval) clearInterval(this.pollingApiInterval);
    if (this.currentEpoch) return;

    try {
      Logger.debug('BlockConsolidationStore: _getCurrentEpochFallback called');
      const {
        currentEpoch,
      }: GetCurrentEpochFallbackResponse = await this.getCurrentEpochFallbackRequest.execute()
        .promise;
      runInAction(() => {
        this.currentEpoch = currentEpoch;
      });
    } catch (error) {
      Logger.error('BlockConsolidationStore: _getCurrentEpochFallback failed', {
        error,
      });
    }
  };
}
