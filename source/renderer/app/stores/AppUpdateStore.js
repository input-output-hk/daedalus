// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { get } from 'lodash';
import semver from 'semver';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { rebuildApplicationMenu } from '../ipc/rebuild-application-menu';
import NewsDomains from '../domains/News';
import {
  requestDownloadChannel,
  getDownloadLocalDataChannel,
  requestResumeDownloadChannel,
  clearDownloadDataChannel,
} from '../ipc/downloadManagerChannel';
import type {
  DownloadMainResponse,
  DownloadsLocalDataMainResponse,
} from '../../../common/ipc/api';
import { DOWNLOAD_EVENT_TYPES } from '../../../common/config/downloadManagerConfig';
import type { GetLatestAppVersionResponse } from '../api/nodes/types';
import type { SoftwareUpdateInfo } from '../api/news/types';

const { version: currentVersion, platform } = global.environment;

// @UPDATE TODO: Dev utils:
/* eslint-disable */
window.semver = semver;

const { News } = NewsDomains;

export type AppUpdateStatus = {
  update: News,
  downloadProgress: number,
};

const APP_UPDATE_DOWNLOAD_ID = 'appUpdate';

export default class AppUpdateStore extends Store {
  @observable availableUpdate: ?News = null;

  @observable isDownloadingUpdate: boolean = false;
  @observable downloadProgress: number = 0;

  @observable isUpdateAvailable: boolean = false;

  @observable isUpdatePostponed: boolean = false;
  @observable isUpdateInstalled: boolean = false;
  // @observable hasPendingDownload: boolean = false;

  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;
  @observable applicationVersion: ?number = null;
  @observable availableUpdates: Array<any> = [];

  // @observable updateFileUrl: ?string = null;
  @observable updateFileUrl: ?string =
    'https://update-cardano-mainnet.iohk.io/daedalus-1.1.0-mainnet-12849.pkg';

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
    requestDownloadChannel.onReceive(this._manageUpdateResponse);

    // ========== MOBX REACTIONS =========== //
    this.registerReactions([this._watchForNewsfeedUpdates]);
  }

  // ================= REACTIONS ==================

  _watchForNewsfeedUpdates = () => {
    const { update } = this.stores.newsFeed.newsFeedData;
    if (update) this._checkNewAppUpdate(update);
  };

  // =============== PRIVATE ===============

  // @UPDATE TODO: Commenting the trigger to avoid automatic download
  _checkNewAppUpdate = async (update: News) => {
    // Is the update valid?
    const isValidUpdate = await this._isUpdateValid(update);
    console.log('isValidUpdate', isValidUpdate);
    if (!isValidUpdate) {
      this._removeLocalDataInfo();
      return;
    }

    runInAction(() => {
      this.availableUpdate = update;
    });

    // Is there a pending / resumable download?
    const unfinishedDownload = await this._getUpdateDownloadLocalData();
    if (unfinishedDownload.data) {
      if (this._isUnfinishedDownloadValid(unfinishedDownload)) {
        // this._requestResumeUpdateDownload();
        return;
      }
    }
    // await this._removeLocalDataInfo();
    // this._requestUpdateDownload(update);
  };

  _isUpdateValid = async (update: News) => {
    const { version: updateVersion } = this.updateInfo;
    return semver.lt(currentVersion, updateVersion);
  };

  _isUnfinishedDownloadValid = async (
    unfinishedDownload: DownloadsLocalDataMainResponse
  ) => {
    console.log('unfinishedDownload', unfinishedDownload);
    return true;
  };

  _removeLocalDataInfo = () => {
    clearDownloadDataChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });
  };

  _getUpdateDownloadLocalData = async (): Promise<DownloadsLocalDataMainResponse> =>
    getDownloadLocalDataChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });

  _manageUpdateResponse = ({
    eventType,
    /* data, */
    progress: progressData,
  }: DownloadMainResponse) => {
    if (eventType === DOWNLOAD_EVENT_TYPES.PROGRESS) {
      const progress = parseInt(progressData.progress, 10);
      runInAction(() => {
        this.downloadProgress = progress;
      });
    }
    runInAction('updates the download information', () => {
      if (eventType === DOWNLOAD_EVENT_TYPES.END) {
        this.isDownloadingUpdate = false;
      } else {
        this.isDownloadingUpdate = true;
      }
    });
    return Promise.resolve({ fileUrl: '' });
  };

  _requestResumeUpdateDownload = async () => {
    await requestResumeDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      options: {
        progressIsThrottled: false,
      },
    });
  };

  _requestUpdateDownload = async (update: News) => {
    const fileUrl = get(update, 'download.darwin');
    if (!fileUrl) return;
    await requestDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      fileUrl,
      options: {
        progressIsThrottled: false,
      },
    });
  };

  // eslint-disable-next-line
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

  // GETTERS

  @computed get updateInfo(): SoftwareUpdateInfo {
    const softwareUpdate = get(this, 'availableUpdate.softwareUpdate', {});
    const { version, hash, url } = softwareUpdate[platform] || {};
    return { version, hash, url };
  }

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
