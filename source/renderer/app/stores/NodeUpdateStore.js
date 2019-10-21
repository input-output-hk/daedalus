// @flow
import { action, computed, observable } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type {
  NodeSoftware,
  GetLatestAppVersionResponse,
} from '../api/nodes/types';
import { NODE_UPDATE_POLL_INTERVAL } from '../config/timingConfig';
import { rebuildApplicationMenu } from '../ipc/rebuild-application-menu';

export default class NodeUpdateStore extends Store {
  @observable isUpdateAvailable = false;
  @observable isUpdatePostponed = false;
  @observable isUpdateInstalled = false;
  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;
  @observable nextUpdateVersion: ?string = null;
  @observable applicationVersion: ?number = null;

  // REQUESTS
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

  setup() {
    const actions = this.actions.nodeUpdate;
    actions.acceptNodeUpdate.listen(this._acceptNodeUpdate);
    actions.postponeNodeUpdate.listen(this._postponeNodeUpdate);
    actions.getLatestAvailableAppVersion.listen(
      this._getLatestAvailableAppVersion
    );
    setInterval(this.refreshNextUpdate, NODE_UPDATE_POLL_INTERVAL);
  }

  refreshNextUpdate = async () => {
    if (this.stores.networkStatus.isSynced) {
      await this.nextUpdateRequest.execute();
      const { result } = this.nextUpdateRequest;
      // If nextUpdate is available, fetch additional Daedalus info
      if (result) {
        await this._getLatestAvailableAppVersion();
        this._activateAutomaticUpdate(result.version);
      }
    }
  };

  @action _activateAutomaticUpdate = async nextUpdateVersion => {
    if (
      nextUpdateVersion &&
      !this.isUpdateAvailable &&
      !this.isUpdatePostponed &&
      !this.isUpdateInstalled
    ) {
      this.isUpdateAvailable = true;
      // If next update version matches applicationVersion (fetched from latestAppVersion json) then set next update version to latest availableAppVersion
      this.nextUpdateVersion =
        nextUpdateVersion === this.applicationVersion
          ? this.availableAppVersion
          : null;

      // Close all active dialogs
      this.stores.app._closeActiveDialog();
      this.actions.app.closeAboutDialog.trigger();

      // Rebuild app menu
      await rebuildApplicationMenu.send({
        isUpdateAvailable: this.isUpdateAvailable,
      });
    }
  };

  /** Automatic update overlay faker
    - example with "newer version" label: _setNextUpdateVersion(11, 10, '0.16.0')
    - example with "v 0.16.0" label: _setNextUpdateVersion(10, 10, '0.16.0')
    */
  @action _setNextUpdateVersion = async (
    nextUpdateVersion,
    applicationVersion,
    availableAppVersion
  ) => {
    this.applicationVersion = applicationVersion;
    this.availableAppVersion = availableAppVersion;
    this._activateAutomaticUpdate(nextUpdateVersion);
  };

  @action _postponeNodeUpdate = async () => {
    this.postponeUpdateRequest.execute();
    this.isUpdatePostponed = true;
    this.isUpdateAvailable = false;
    await rebuildApplicationMenu.send({
      isUpdateAvailable: this.isUpdateAvailable,
    });
  };

  @action _acceptNodeUpdate = async () => {
    this.applyUpdateRequest.execute();
  };

  @action hideUpdateDialog = async () => {
    this.isUpdateInstalled = true;
    this.isUpdateAvailable = false;
  };

  @action _getLatestAvailableAppVersion = async () => {
    const {
      latestAppVersion,
      applicationVersion,
    } = await this.getLatestAppVersionRequest.execute().promise;
    this.setLatestAvailableAppVersion(latestAppVersion, applicationVersion);
  };

  @action setLatestAvailableAppVersion = (
    latestAppVersion: ?string,
    applicationVersion: ?number
  ) => {
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
    this.applicationVersion = applicationVersion;
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

  @computed get showNextUpdate(): boolean {
    return (
      this.isUpdateAvailable &&
      !this.isUpdatePostponed &&
      !this.isUpdateInstalled
    );
  }
}
