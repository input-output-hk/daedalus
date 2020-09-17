// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { get } from 'lodash';
import semver from 'semver';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import NewsDomains from '../domains/News';
import { logger } from '../utils/logging';
import {
  requestDownloadChannel,
  requestResumeDownloadChannel,
  deleteDownloadedFile,
  getDownloadLocalDataChannel,
  clearDownloadLocalDataChannel,
  checkFileExistsChannel,
} from '../ipc/downloadManagerChannel';
import { quitAppAndAppInstallUpdateChannel } from '../ipc/quitAppAndAppInstallUpdateChannel';
import type {
  DownloadMainResponse,
  DownloadLocalDataMainResponse,
  CheckFileExistsMainResponse,
  QuitAppAndAppInstallUpdateRendererRequest,
  QuitAppAndAppInstallUpdateMainResponse,
} from '../../../common/ipc/api';
import { formattedDownloadData } from '../utils/formatters.js';
import {
  DOWNLOAD_EVENT_TYPES,
  DOWNLOAD_STATES,
} from '../../../common/config/downloadManagerConfig';
import type { SoftwareUpdateInfo } from '../api/news/types';
import type {
  DownloadInfo,
  DownloadData,
} from '../../../common/types/downloadManager.types';
import type { FormattedDownloadData } from '../utils/formatters.js';

const { version: currentVersion, platform } = global.environment;
const { News } = NewsDomains;
const APP_UPDATE_DOWNLOAD_ID = 'appUpdate';

export default class AppUpdateStore extends Store {
  @observable availableUpdate: ?News = null;
  @observable availableUpdateVersion: string = '';
  @observable isUpdateDownloading: boolean = false;
  @observable isUpdateDownloaded: boolean = false;
  @observable isUpdateInstalled: boolean = false;
  @observable isUpdateProgressOpen: boolean = false;
  @observable isAutomaticUpdateFailed: boolean = false;
  @observable isUpdatePostponed: boolean = false;
  @observable isInstallingUpdate: boolean = false;

  @observable downloadInfo: ?DownloadInfo = null;
  @observable downloadData: ?DownloadData = null;
  @observable availableAppVersion: ?string = null;

  @observable getAppAutomaticUpdateFailedRequest: Request<
    Promise<boolean>
  > = new Request(this.api.localStorage.getAppAutomaticUpdateFailed);
  @observable setAppAutomaticUpdateFailedRequest: Request<
    Promise<void>
  > = new Request(this.api.localStorage.setAppAutomaticUpdateFailed);
  @observable unsetAppAutomaticUpdateFailedRequest: Request<
    Promise<void>
  > = new Request(this.api.localStorage.unsetAppAutomaticUpdateFailed);

  @observable getAppUpdateCompletedRequest: Request<
    Promise<boolean>
  > = new Request(this.api.localStorage.getAppUpdateCompleted);
  @observable setAppUpdateCompletedRequest: Request<
    Promise<void>
  > = new Request(this.api.localStorage.setAppUpdateCompleted);
  @observable unsetAppUpdateCompletedRequest: Request<
    Promise<void>
  > = new Request(this.api.localStorage.unsetAppUpdateCompleted);

  setup() {
    const actions = this.actions.appUpdate;
    actions.installUpdate.listen(this._installUpdate);
    actions.openAppUpdateOverlay.listen(this._openAppUpdateOverlay);
    actions.closeAppUpdateOverlay.listen(this._closeAppUpdateOverlay);
    actions.postponeUpdate.listen(this._postponeUpdate);

    requestDownloadChannel.onReceive(this._manageUpdateResponse);
    quitAppAndAppInstallUpdateChannel.onReceive(
      this._manageQuitAndInstallResponse
    );

    // ============== MOBX REACTIONS ==============
    this.registerReactions([this._watchForNewsfeedUpdates]);
  }

  // ================= REACTIONS ==================

  _watchForNewsfeedUpdates = () => {
    const { update } = this.stores.newsFeed.newsFeedData;
    if (update) this._checkNewAppUpdate(update);
  };

  // ==================== PUBLIC ==================

  @computed get displayAppUpdateOverlay(): boolean {
    return (
      !!this.availableUpdate &&
      !this.isUpdatePostponed &&
      (this.isUpdateProgressOpen ||
        this.isUpdateDownloaded ||
        this.isAutomaticUpdateFailed)
    );
  }
  @computed get displayAppUpdateNewsItem(): boolean {
    return this.isUpdateDownloading || this.isUpdatePostponed;
  }

  @computed get formattedDownloadData(): FormattedDownloadData {
    return formattedDownloadData(
      this.downloadData,
      this.stores.profile.currentLocale
    );
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

  @computed get showManualUpdate(): boolean {
    return this.isAutomaticUpdateFailed;
  }

  @computed get isUpdateAvailable(): boolean {
    return this.availableUpdate !== null;
  }

  getUpdateInfo(update: News): SoftwareUpdateInfo {
    const softwareUpdate = get(update, 'softwareUpdate', {});
    const { version, hash, url } = softwareUpdate[platform] || {};
    return { version, hash, url };
  }

  isUpdateInstalled = (update: News) => {
    const { version: updateVersion } = this.getUpdateInfo(update);
    return !semver.lt(currentVersion, updateVersion);
  };

  // =================== PRIVATE ==================

  _checkNewAppUpdate = async (update: News) => {
    // Cancels if the update download is already in progress
    if (this.isUpdateDownloading) return false;
    const { version } = this.getUpdateInfo(update);
    const appUpdateCompleted = await this.getAppUpdateCompletedRequest.execute();

    /*
     * The update was already installed and the installer was already deleted.
     * We can't simply compare with the `package.json` version
     * otherwise we would trigger the localdata cleaning on every app load
     */
    if (appUpdateCompleted === version) return false;

    // Was the update already installed?
    if (this.isUpdateInstalled(update)) {
      // Sets the `appUpdateCompleted` flag to prevent this whole process every app load
      await this.setAppUpdateCompletedRequest.execute(version);
      await this.unsetAppAutomaticUpdateFailedRequest.execute();
      await this._removeUpdateFile();
      await this._removeLocalDataInfo();
      return false;
    }

    // The Update is valid and needs to be downloaded/installed
    runInAction(() => {
      this.availableUpdate = update;
      this.availableUpdateVersion = version;
    });

    // Is there an 'Automatic Update Failed' flag?
    const isAutomaticUpdateFailed = await this.getAppAutomaticUpdateFailedRequest.execute();
    if (isAutomaticUpdateFailed) {
      runInAction(() => {
        this.isAutomaticUpdateFailed = true;
      });
      return false;
    }

    // Is there a pending / resumable download?
    const downloadLocalData = await this._getUpdateDownloadLocalData();
    const { info, data } = downloadLocalData;
    if (info && data) {
      // The user reopened Daedalus without installing the update
      if (data.state === DOWNLOAD_STATES.FINISHED && data.progress === 100) {
        // Does the file still exist?
        const installerFileStillExists = await this._checkFileExists();
        if (!installerFileStillExists) {
          logger.error(
            'AppUpdateStore:_setAppAutomaticUpdateFailed: Failed to find the installer file'
          );
          this._setAppAutomaticUpdateFailed();
          return false;
        }

        runInAction(() => {
          this.downloadInfo = info;
          this.downloadData = data;
          this.isUpdateDownloaded = true;
        });
        return false;
      }

      // Resumes the update download
      this._requestResumeUpdateDownload();
      return false;
    }

    await this._removeLocalDataInfo();
    return this._requestUpdateDownload(update);
  };

  _removeLocalDataInfo = async () => {
    clearDownloadLocalDataChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });
  };

  _removeUpdateFile = () => {
    deleteDownloadedFile.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });
  };

  _getUpdateDownloadLocalData = async (): Promise<DownloadLocalDataMainResponse> =>
    getDownloadLocalDataChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });

  _checkFileExists = async (): Promise<CheckFileExistsMainResponse> =>
    checkFileExistsChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
    });

  _manageUpdateResponse = ({
    eventType,
    info,
    data,
    error,
  }: DownloadMainResponse) => {
    runInAction('updates the download information', () => {
      if (eventType === DOWNLOAD_EVENT_TYPES.PAUSE) {
        this.availableUpdate = null;
        this.isUpdateDownloaded = true;
      }
      if (eventType === DOWNLOAD_EVENT_TYPES.PROGRESS) {
        this.downloadInfo = info;
        this.downloadData = data;
      }
      if (eventType === DOWNLOAD_EVENT_TYPES.END) {
        this.isUpdateDownloaded = true;
        this.actions.app.closeNewsFeed.trigger();
      }
      if (eventType === DOWNLOAD_EVENT_TYPES.ERROR) {
        logger.error(
          'AppUpdateStore:_setAppAutomaticUpdateFailed: Received an error event from the main process',
          {
            error,
          }
        );
        this._setAppAutomaticUpdateFailed();
      }
      if (
        eventType === DOWNLOAD_EVENT_TYPES.END ||
        eventType === DOWNLOAD_EVENT_TYPES.PAUSE ||
        eventType === DOWNLOAD_EVENT_TYPES.ERROR
      ) {
        this.isUpdateDownloading = false;
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

  _requestUpdateDownload = (update: News) => {
    const { url: fileUrl } = this.getUpdateInfo(update);
    if (!fileUrl) return null;
    return requestDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      fileUrl,
      options: {
        progressIsThrottled: false,
        persistLocalData: true,
      },
    });
  };

  _installUpdate = async () => {
    if (
      !this.availableUpdate ||
      this.isUpdateDownloading ||
      !this.isUpdateDownloaded ||
      !this.downloadInfo
    ) {
      logger.error(
        'AppUpdateStore:_setAppAutomaticUpdateFailed: Unable to install the update',
        {
          '!this.availableUpdate': !this.availableUpdate,
          'this.isUpdateDownloading': this.isUpdateDownloading,
          '!this.isUpdateDownloaded': !this.isUpdateDownloaded,
          '!this.downloadInfo': !this.downloadInfo,
        }
      );
      // this._setAppAutomaticUpdateFailed();
      return false;
    }
    runInAction(() => {
      this.isInstallingUpdate = true;
    });
    if (!this.availableUpdate) return false;
    const { destinationPath, originalFilename } = this.downloadInfo || {};
    const { hash } = this.getUpdateInfo(this.availableUpdate);
    const filePath = `${destinationPath}/${originalFilename}`;
    return quitAppAndAppInstallUpdateChannel.request({
      filePath,
      hash,
    });
  };

  _manageQuitAndInstallResponse = ({
    status,
    message,
    data,
  }: QuitAppAndAppInstallUpdateMainResponse) => {
    if (status === 'error') {
      logger.error(message || '', { data });
      runInAction(() => {
        this.isInstallingUpdate = false;
      });
      this._setAppAutomaticUpdateFailed();
    } else {
      // eslint-disable-next-line
      console.log('--- INSTALLATION ', {
        status,
        message,
        data,
      });
    }
    return Promise.resolve({ filePath: '', hash: '' });
  };

  _setAppAutomaticUpdateFailed = async () => {
    await this.setAppAutomaticUpdateFailedRequest.execute();
    runInAction(() => {
      this.isAutomaticUpdateFailed = true;
    });
  };

  @action _openAppUpdateOverlay = () => {
    this.isUpdateProgressOpen = true;
    this.isUpdatePostponed = false;
  };

  @action _closeAppUpdateOverlay = () => {
    this.isUpdateProgressOpen = false;
  };

  @action _postponeUpdate = () => {
    this.isUpdatePostponed = true;
  };
}
