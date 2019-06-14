// @flow
import { action, computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type {
  NodeSoftware,
  GetLatestAppVersionResponse,
} from '../api/nodes/types';
import { NODE_UPDATE_POLL_INTERVAL } from '../config/timingConfig';

export default class NodeUpdateStore extends Store {
  @observable isUpdateAvailable = false;
  @observable isUpdatePostponed = false;
  @observable isNotificationExpanded = false;
  @observable isUpdateInstalled = false;
  @observable updateVersion = null;
  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;

  // REQUESTS
  /* eslint-disable max-len */
  @observable nextUpdateRequest: Request<NodeSoftware> = new Request(
    this.api.ada.nextUpdate
  );
  @observable postponeUpdateRequest: Request<Promise<void>> = new Request(
    this.api.ada.postponeUpdate
  );
  @observable applyUpdateRequest: Request<Promise<void>> = new Request(
    this.api.ada.applyUpdate
  );
  @observable
  getLatestAppVersionRequest: Request<GetLatestAppVersionResponse> = new Request(
    this.api.ada.getLatestAppVersion
  );
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.nodeUpdate;
    actions.acceptNodeUpdate.listen(this._acceptNodeUpdate);
    actions.postponeNodeUpdate.listen(this._postponeNodeUpdate);
    actions.toggleNodeUpdateNotificationExpanded.listen(
      this._toggleNotificationExpanded
    );
    actions.getLatestAvailableAppVersion.listen(
      this._getLatestAvailableAppVersion
    );
    setInterval(this.refreshNextUpdate, NODE_UPDATE_POLL_INTERVAL);
  }

  refreshNextUpdate = async () => {
    if (this.stores.networkStatus.isSynced) {
      await this.nextUpdateRequest.execute();
      const { result } = this.nextUpdateRequest;
      if (
        result &&
        !this.isUpdateAvailable &&
        !this.isUpdatePostponed &&
        !this.isUpdateInstalled
      ) {
        runInAction('refreshNextUpdate', () => {
          this.isUpdateAvailable = true;
          this.isNotificationExpanded = true;
          this.updateVersion = result.version;
        });
      }
    }
  };

  @action _postponeNodeUpdate = () => {
    this.postponeUpdateRequest.execute();
    this.isUpdatePostponed = true;
  };

  @action _acceptNodeUpdate = () => {
    this.applyUpdateRequest.execute();
    this.isUpdateAvailable = false;
    this.isUpdateInstalled = true;
  };

  @action _toggleNotificationExpanded = () => {
    this.isNotificationExpanded = !this.isNotificationExpanded;
  };

  @action _getLatestAvailableAppVersion = async () => {
    const { latestAppVersion } = await this.getLatestAppVersionRequest.execute()
      .promise;
    this.setLatestAvailableAppVersion(latestAppVersion);
  };

  @action setLatestAvailableAppVersion = (latestAppVersion: ?string) => {
    let isNewAppVersionAvailable = false;

    if (latestAppVersion) {
      const { version: currentVersion } = this.environment;
      const chunkedCurrentVersion = currentVersion.split('.').map(Number);
      const chunkedLatestVersion = latestAppVersion.split('.').map(Number);

      // Main version changed
      const isMainVersionChanged =
        chunkedCurrentVersion[0] < chunkedLatestVersion[0];
      // Middle version changed
      const isMiddleVersionChanged =
        chunkedCurrentVersion[0] === chunkedLatestVersion[0] &&
        chunkedCurrentVersion[1] < chunkedLatestVersion[1];
      // Minor version changed
      const isMinorVersionChanged =
        chunkedCurrentVersion[0] === chunkedLatestVersion[0] &&
        chunkedCurrentVersion[1] === chunkedLatestVersion[1] &&
        chunkedCurrentVersion[2] < chunkedLatestVersion[2];
      isNewAppVersionAvailable =
        isMainVersionChanged || isMiddleVersionChanged || isMinorVersionChanged;
    }

    this.isNewAppVersionAvailable = isNewAppVersionAvailable;
    this.availableAppVersion = latestAppVersion;
  };

  @computed get isNewAppVersionLoading(): boolean {
    return this.getLatestAppVersionRequest.isExecuting;
  }

  @computed get isNewAppVersionLoaded(): boolean {
    return (
      this.getLatestAppVersionRequest.wasExecuted &&
      (this.getLatestAppVersionRequest.result !== null ||
        this.getLatestAppVersionRequest.error !== null)
    );
  }
}
