// @flow
import { observable, action, runInAction } from 'mobx';
import remotedev from 'mobx-remotedev/lib/dev';
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

@remotedev
export default class BlockConsolidationStore extends Store {
  // Initialize store observables
  @observable epochsConsolidated: number = 0; // Received from the IPC channel
  @observable currentEpoch: number = 0; // Received from the API (or from cardanoexplorer when the API fails)

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
    actions.stopBlockConsolidationDataPolling.listen(
      this._stopBlockConsolidationDataPolling
    );
    actions.startBlockConsolidationDataPolling.listen(
      this._startBlockConsolidationDataPolling
    );
  }

  _startBlockConsolidationDataPolling = () => {
    this._getBlockConsolidationIpcData();
    this._getBlockConsolidationApiData();
    this._stopBlockConsolidationDataPolling();

    this.pollingIpcInterval = setInterval(
      this._getBlockConsolidationIpcData,
      BLOCK_CONSOLIDATION_IPC_REQUEST_INTERVAL
    );
    this.pollingApiInterval = setInterval(
      this._getBlockConsolidationApiData,
      BLOCK_CONSOLIDATION_API_REQUEST_INTERVAL
    );
  };

  _stopBlockConsolidationDataPolling = (options: { apiOnly: boolean } = {}) => {
    const { apiOnly } = options;
    if (this.pollingIpcInterval && !apiOnly)
      clearInterval(this.pollingIpcInterval);
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
    if (!this.stores.networkStatus.tlsConfig) {
      this._stopBlockConsolidationDataPolling({ apiOnly: true });
      this.actions.networkStatus.tlsConfigIsReady.listen(
        this._startBlockConsolidationDataPolling
      );
      return false;
    }
    try {
      Logger.debug(
        'BlockConsolidationStore: _getBlockConsolidationApiData called'
      );
      const nodeSettings: GetNodeSettingsResponse = await this.getNodeSettingsRequest.execute()
        .promise;
      const { slotId } = nodeSettings;
      if (slotId && slotId.epoch) {
        return runInAction('current epoch', () => {
          this.currentEpoch = slotId.epoch;
        });
      }
      Logger.error(
        'BlockConsolidationStore: _getBlockConsolidationApiData failed',
        { error: 'API did not return "slotId"' }
      );
      return this._getCurrentEpochFallback();
    } catch (error) {
      Logger.error(
        'BlockConsolidationStore: _getBlockConsolidationApiData failed',
        { error }
      );
      return this._getCurrentEpochFallback();
    }
  };

  @action _getCurrentEpochFallback = async () => {
    this._stopBlockConsolidationDataPolling({ apiOnly: true });
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
