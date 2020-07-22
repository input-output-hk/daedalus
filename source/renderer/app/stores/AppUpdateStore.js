// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { get } from 'lodash';
import semver from 'semver';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import NewsDomains from '../domains/News';
import {
  requestDownloadChannel,
  requestResumeDownloadChannel,
  deleteDownloadedFile,
  getDownloadLocalDataChannel,
  clearDownloadLocalDataChannel,
} from '../ipc/downloadManagerChannel';
import { quitAppInstallUpdateChannel } from '../ipc/quitAppInstallUpdateChannel';
import type {
  DownloadMainResponse,
  DownloadLocalDataMainResponse,
} from '../../../common/ipc/api';
import { formattedDownloadData } from '../utils/formatters.js';
import {
  DOWNLOAD_EVENT_TYPES,
  DOWNLOAD_STATES,
} from '../../../common/config/downloadManagerConfig';
import type { GetLatestAppVersionResponse } from '../api/nodes/types';
import type { SoftwareUpdateInfo } from '../api/news/types';
import type {
  DownloadInfo,
  DownloadData,
} from '../../../common/types/downloadManager.types';
import type { FormattedDownloadData } from '../utils/formatters.js';

const { version: currentVersion, platform } = global.environment;

// @UPDATE TODO: Dev utils:
/* eslint-disable */
window.semver = semver;

const { News } = NewsDomains;

const dummyInfo: DownloadInfo = {
  downloadId: 'appUpdate',
  fileUrl:
    'https://update-cardano-mainnet.iohk.io/daedalus-1.1.0-mainnet-12849.pkg',
  destinationPath:
    '/Users/danilo/Library/Application Support/Daedalus Shelley Testnet v4/Downloads',
  destinationDirectoryName: 'stateDirectory',
  temporaryFilename: 'Unconfirmed-2020-07-22T180223.0689Z.crdownload',
  originalFilename: 'daedalus-1.1.0-mainnet-12849.pkg',
  options: {
    progressIsThrottled: false,
    persistLocalData: true,
    fileName: 'Unconfirmed-2020-07-22T180223.0689Z.crdownload',
  },
};
const dummyData: DownloadData = {
  state: 'DOWNLOADING',
  remainingSize: 130699883,
  serverFileSize: 229305198,
  diskFileSize: 0,
  downloadSize: 98605315,
  progress: 43.00177922700208,
  speed: 3751936,
  incomplete: false,
  isResumed: false,
};

const APP_UPDATE_DOWNLOAD_ID = 'appUpdate';

export default class AppUpdateStore extends Store {
  @observable availableUpdate: ?News = null;
  @observable availableUpdateVersion: string = '';
  // @UPDATE TODO
  // @observable isUpdateDownloading: boolean = true;
  @observable isUpdateDownloading: boolean = false;
  @observable isUpdateDownloaded: boolean = false;
  // @UPDATE TODO
  // @observable downloadInfo: ?DownloadInfo = dummyInfo;
  @observable downloadInfo: ?DownloadInfo = null;
  // @UPDATE TODO
  // @observable downloadData: ?DownloadData = dummyData;
  @observable downloadData: ?DownloadData = null;

  @observable isUpdateAvailable: boolean = false;
  @observable isUpdateInstalled: boolean = false;
  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;
  @observable applicationVersion: ?number = null;

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
    const actions = this.actions.appUpdate;
    actions.getLatestAvailableAppVersion.listen(
      this._getLatestAvailableAppVersion
    );
    actions.onInstallUpdate.listen(this._onInstallUpdate);

    requestDownloadChannel.onReceive(this._manageUpdateResponse);

    // ============== MOBX REACTIONS ==============
    this.registerReactions([this._watchForNewsfeedUpdates]);
  }

  // ================= REACTIONS ==================

  _watchForNewsfeedUpdates = () => {
    const { update } = this.stores.newsFeed.newsFeedData;
    if (update) this._checkNewAppUpdate(update);
  };

  // ==================== PUBLIC ==================

  @computed get formattedDownloadData(): FormattedDownloadData {
    return formattedDownloadData(this.downloadData);
  }

  @computed get downloadTimeLeft(): string {
    return this.formattedDownloadData.timeLeft;
  }

  @computed get totalDownloaded(): string {
    return this.formattedDownloadData.downloaded;
  }

  @computed get totalDownloadSize(): string {
    return this.formattedDownloadData.total;
  }

  @computed get downloadProgress(): number {
    return this.formattedDownloadData.progress;
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

  @computed get showManualUpdate(): boolean {
    return (
      this.isNewAppVersionAvailable &&
      !this.isUpdateAvailable &&
      !global.isIncentivizedTestnet &&
      !global.isFlight
    );
  }

  getUpdateInfo(update: News): SoftwareUpdateInfo {
    const softwareUpdate = get(update, 'softwareUpdate', {});
    const { version, hash, url } = softwareUpdate[platform] || {};
    return { version, hash, url };
  }

  isUpdateValid = (update: News) => {
    const { version: updateVersion } = this.getUpdateInfo(update);
    return semver.lt(currentVersion, updateVersion);
  };

  isUnfinishedDownloadValid = async (
    unfinishedDownload: DownloadLocalDataMainResponse
  ) => {
    return true;
  };

  // =================== PRIVATE ==================

  // @UPDATE TODO: Remove it
  @action _toggleUsUpdateDownloaded = () =>
    (this.isUpdateDownloaded = !this.isUpdateDownloaded);

  // @UPDATE TODO: Commenting the trigger to avoid automatic download
  _checkNewAppUpdate = async (update: News) => {
    // Is the update valid?
    if (!this.isUpdateValid(update)) {
      await this._removeUpdateFile();
      await this._removeLocalDataInfo();
      return;
    }

    const { version } = this.getUpdateInfo(update);

    runInAction(() => {
      this.availableUpdate = update;
      this.availableUpdateVersion = version;
    });

    // Is there a pending / resumabl\e download?
    const downloadLocalData = await this._getUpdateDownloadLocalData();
    const { info, data } = downloadLocalData;
    if (info && data) {
      if (data.state === DOWNLOAD_STATES.FINISHED && data.progress === 100) {
        runInAction(() => {
          this.downloadInfo = info;
          this.downloadData = data;
          this.isUpdateDownloaded = true;
        });
        return;
      }

      if (this.isUnfinishedDownloadValid(downloadLocalData)) {
        // this._requestResumeUpdateDownload();
        return;
      }
    }
    // await this._removeLocalDataInfo();
    // this._requestUpdateDownload(update);
  };

  _removeLocalDataInfo = async () => {
    clearDownloadLocalDataChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });
  };

  // @UPDATE TODO: Implement this method
  _removeUpdateFile = () => {
    deleteDownloadedFile.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });
  };

  _getUpdateDownloadLocalData = async (): Promise<DownloadLocalDataMainResponse> =>
    getDownloadLocalDataChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });

  _manageUpdateResponse = ({ eventType, info, data }: DownloadMainResponse) => {
    if (eventType === DOWNLOAD_EVENT_TYPES.PROGRESS) {
      runInAction(() => {
        this.downloadInfo = info;
        this.downloadData = data;
      });
    }
    runInAction('updates the download information', () => {
      if (eventType === DOWNLOAD_EVENT_TYPES.END) {
        this.isUpdateDownloading = false;
        this.isUpdateDownloaded = true;
      } else {
        this.isUpdateDownloading = true;
      }
    });
    return Promise.resolve({ fileUrl: '' });
  };

  _requestResumeUpdateDownload = async () => {
    await requestResumeDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      options: {
        progressIsThrottled: false,
        persistLocalData: true,
      },
    });
  };

  _requestUpdateDownload = async (update: News) => {
    const { url: fileUrl } = this.getUpdateInfo(update);
    if (!fileUrl) return console.warn('File not found');
    await requestDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      fileUrl,
      options: {
        progressIsThrottled: false,
        persistLocalData: true,
      },
    });
  };

  @action _onInstallUpdate = async () => {
    if (
      !this.availableUpdate ||
      this.isUpdateDownloading ||
      !this.isUpdateDownloaded ||
      !this.downloadInfo
    ) {
      console.log('!this.availableUpdate', !this.availableUpdate);
      console.log('this.isUpdateDownloading', this.isUpdateDownloading);
      console.log('!this.isUpdateDownloaded', !this.isUpdateDownloaded);
      console.log('!this.downloadInfo', !this.downloadInfo);
      return;
    }
    const { destinationPath, originalFilename } = this.downloadInfo;
    const filePath = `${destinationPath}/${originalFilename}`;
    quitAppInstallUpdateChannel.request(filePath);
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
}
