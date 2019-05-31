// @flow
import { observable, action, runInAction } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type { NodeSoftware } from '../api/nodes/types';
import { NODE_UPDATE_POLL_INTERVAL } from '../config/timingConfig';

export default class NodeUpdateStore extends Store {

  @observable isUpdateAvailable = false;
  @observable isUpdatePostponed = false;
  @observable isNotificationExpanded = false;
  @observable isUpdateInstalled = false;
  @observable updateVersion = null;
  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;
  @observable isNewAppVersionLoading: boolean = false;

  // REQUESTS
  /* eslint-disable max-len */
  @observable nextUpdateRequest: Request<NodeSoftware> = new Request(this.api.ada.nextUpdate);
  @observable postponeUpdateRequest: Request<Promise<void>> = new Request(this.api.ada.postponeUpdate);
  @observable applyUpdateRequest: Request<Promise<void>> = new Request(this.api.ada.applyUpdate);
  /* eslint-disable max-len */

  setup() {
    const actions = this.actions.nodeUpdate;
    actions.acceptNodeUpdate.listen(this._acceptNodeUpdate);
    actions.postponeNodeUpdate.listen(this._postponeNodeUpdate);
    actions.toggleNodeUpdateNotificationExpanded.listen(this._toggleNotificationExpanded);
    actions.getLatestAvailableAppVersion.listen(this._getLatestAvailableAppVersion);
    setInterval(this.refreshNextUpdate, NODE_UPDATE_POLL_INTERVAL);
  }

  refreshNextUpdate = async () => {
    if (this.stores.networkStatus.isSynced) {
      await this.nextUpdateRequest.execute();
      const { result } = this.nextUpdateRequest;
      if (result && !this.isUpdateAvailable && !this.isUpdatePostponed && !this.isUpdateInstalled) {
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
    const { version, platform, isDevelopment } = this.environment;

    if (!isDevelopment) {
      this.isNewAppVersionLoading = true;
      const versionInfo = await this.api.ada.getLatestAppVersionInfo();
      const availableVersion = get(versionInfo, ['platforms', platform, 'version'], null);
      const isNewAppVersionAvailable = availableVersion && availableVersion > version;

      runInAction(() => {
        this.isNewAppVersionLoading = false;
        this.isNewAppVersionAvailable = isNewAppVersionAvailable;
        this.availableAppVersion = availableVersion;
      });
    }
  }

}
