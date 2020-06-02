// @flow
import { action, computed, observable, runInAction } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type { AppInfo, GetLatestAppVersionResponse } from '../api/nodes/types';
import { APP_UPDATE_POLL_INTERVAL } from '../config/timingConfig';
import { rebuildApplicationMenu } from '../ipc/rebuild-application-menu';
import {
  requestDownloadChannel,
  getDownloadLocalDataChannel,
  requestResumeDownloadChannel,
} from '../ipc/downloadManagerChannel';
import type { DownloadMainResponse } from '../../../common/ipc/api';
import { DOWNLOAD_EVENT_TYPES } from '../../../common/config/download-manager';

export default class AppUpdateStore extends Store {
  @observable isUpdateAvailable = false;
  @observable isDownloadingUpdate = false;
  @observable isUpdatePostponed = false;
  @observable isUpdateInstalled = false;
  @observable hasPendingDownload = false;
  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;
  @observable nextUpdateVersion: ?string = null;
  @observable applicationVersion: ?number = null;
  @observable downloadProgress: ?number = null;

  // REQUESTS
  @observable nextUpdateRequest: Request<AppInfo> = new Request(
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

  nextUpdateInterval: ?IntervalID = null;

  setup() {
    const actions = this.actions.appUpdate;
    actions.acceptAppUpdate.listen(this._acceptAppUpdate);
    actions.postponeAppUpdate.listen(this._postponeAppUpdate);
    actions.getLatestAvailableAppVersion.listen(
      this._getLatestAvailableAppVersion
    );

    const { isIncentivizedTestnet, isFlight } = global;
    if (!isFlight && !isIncentivizedTestnet) {
      this.nextUpdateInterval = setInterval(
        this.refreshNextUpdate,
        APP_UPDATE_POLL_INTERVAL
      );
    }

    // this.managePendingUpdate();
  }

  managePendingUpdate = async () => {
    const downloadLocalData = await getDownloadLocalDataChannel.request({
      id: 'appUpdate',
    });
    return downloadLocalData;
  };

  requestResumeDownload = async (id: string = 'appUpdate') => {
    await requestResumeDownloadChannel.request({
      id,
    });
  };

  _requestDownload = async (
    fileUrl: string = 'https://update-cardano-mainnet.iohk.io/daedalus-1.1.0-mainnet-12849.pkg'
  ) => {
    requestDownloadChannel.onReceive(
      ({ eventType, data, progress }: DownloadMainResponse) => {
        console.log('progress', progress.progress);
        runInAction('updates the download information', () => {
          if (eventType === DOWNLOAD_EVENT_TYPES.END) {
            this.isDownloadingUpdate = false;
          } else {
            this.isDownloadingUpdate = true;
          }
        });
        return Promise.resolve({ fileUrl: '' });
      }
    );
    await requestDownloadChannel.request({
      id: 'appUpdate',
      fileUrl,
    });
  };

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

  @action _activateAutomaticUpdate = async (nextUpdateVersion?: string) => {
    if (
      nextUpdateVersion &&
      !this.isUpdateAvailable &&
      !this.isUpdatePostponed &&
      !this.isUpdateInstalled
    ) {
      this.isUpdateAvailable = true;
      // If next update version matches applicationVersion (fetched from latestAppVersion json)
      // then set next update version to latest availableAppVersion
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

  @action _postponeAppUpdate = async () => {
    this.postponeUpdateRequest.execute();
    this.isUpdatePostponed = true;
    this.isUpdateAvailable = false;
    await rebuildApplicationMenu.send({
      isUpdateAvailable: this.isUpdateAvailable,
    });
  };

  @action _acceptAppUpdate = async () => {
    this.applyUpdateRequest.execute();
  };

  @action hideUpdateDialog = async () => {
    this.isUpdateInstalled = true;
    this.isUpdateAvailable = false;
  };

  @action _getLatestAvailableAppVersion = async () => {
    // Manual update notification is not available for Daedalus Flight and ITN builds
    const { isIncentivizedTestnet, isFlight } = global;
    if (isFlight || isIncentivizedTestnet) {
      return;
    }

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
      !this.isUpdateInstalled &&
      !global.isIncentivizedTestnet &&
      !global.isFlight
    );
  }

  @computed get showManualUpdate(): boolean {
    return (
      this.isNewAppVersionAvailable &&
      !this.isUpdatePostponed &&
      !this.isUpdateAvailable &&
      !global.isIncentivizedTestnet &&
      !global.isFlight
    );
  }
}
