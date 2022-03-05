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
import { manageAppUpdateChannel } from '../ipc/manageAppUpdateChannel';
import type {
  DownloadMainResponse,
  DownloadLocalDataMainResponse,
  CheckFileExistsMainResponse,
  ManageAppUpdateMainResponse,
} from '../../../common/ipc/api';
import {
  APP_UPDATE_DOWNLOAD_ID,
  UPDATE_INSTALLATION_STATUSES as statuses,
} from '../../../common/config/appUpdateConfig';
import { formattedDownloadData } from '../utils/formatters';
import {
  DOWNLOAD_EVENT_TYPES,
  DOWNLOAD_STATES,
} from '../../../common/config/downloadManagerConfig';
import type { SoftwareUpdateInfo } from '../api/news/types';
import type {
  DownloadInfo,
  DownloadData,
} from '../../../common/types/downloadManager.types';
import type { FormattedDownloadData } from '../utils/formatters';

const { version: currentVersion, platform } = global.environment;
const { News } = NewsDomains;
export default class AppUpdateStore extends Store {
  @observable
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  availableUpdate: News | null | undefined = null;
  @observable
  availableUpdateVersion = '';
  @observable
  isUpdateDownloading = false;
  @observable
  isUpdateDownloaded = false;
  @observable
  isUpdateProgressOpen = false;
  @observable
  isAutomaticUpdateFailed = false;
  @observable
  isUpdatePostponed = false;
  @observable
  isWaitingToQuitDaedalus = false;
  @observable
  installationProgress = 0;
  @observable
  downloadInfo: DownloadInfo | null | undefined = null;
  @observable
  downloadData: DownloadData | null | undefined = null;
  @observable
  availableAppVersion: string | null | undefined = null;
  @observable
  getAppAutomaticUpdateFailedRequest: Request<Promise<boolean>> = new Request(
    this.api.localStorage.getAppAutomaticUpdateFailed
  );
  @observable
  setAppAutomaticUpdateFailedRequest: Request<Promise<void>> = new Request(
    this.api.localStorage.setAppAutomaticUpdateFailed
  );
  @observable
  unsetAppAutomaticUpdateFailedRequest: Request<Promise<void>> = new Request(
    this.api.localStorage.unsetAppAutomaticUpdateFailed
  );
  @observable
  getAppUpdateCompletedRequest: Request<Promise<boolean>> = new Request(
    this.api.localStorage.getAppUpdateCompleted
  );
  @observable
  setAppUpdateCompletedRequest: Request<Promise<void>> = new Request(
    this.api.localStorage.setAppUpdateCompleted
  );
  @observable
  unsetAppUpdateCompletedRequest: Request<Promise<void>> = new Request(
    this.api.localStorage.unsetAppUpdateCompleted
  );

  setup() {
    const actions = this.actions.appUpdate;
    actions.installUpdate.listen(this._installUpdate);
    actions.openAppUpdateOverlay.listen(this._openAppUpdateOverlay);
    actions.closeAppUpdateOverlay.listen(this._closeAppUpdateOverlay);
    actions.postponeUpdate.listen(this._postponeUpdate);
    requestDownloadChannel.onReceive(this._manageUpdateResponse);
    manageAppUpdateChannel.onReceive(this._manageQuitAndInstallResponse);
    // ============== MOBX REACTIONS ==============
    this.registerReactions([this._watchForNewsfeedUpdates]);
  }

  // ================= REACTIONS ==================
  _watchForNewsfeedUpdates = () => {
    const { update } = this.stores.newsFeed.newsFeedData;
    if (update) this._checkNewAppUpdate(update);
  };

  // ==================== PUBLIC ==================
  @computed
  get displayAppUpdateOverlay(): boolean {
    return (
      !!this.availableUpdate &&
      !this.isUpdatePostponed &&
      (this.isUpdateProgressOpen ||
        this.isUpdateDownloaded ||
        this.isAutomaticUpdateFailed)
    );
  }

  @computed
  get displayAppUpdateNewsItem(): boolean {
    return this.isUpdateDownloading || this.isUpdatePostponed;
  }

  @computed
  get formattedDownloadData(): FormattedDownloadData {
    return formattedDownloadData(
      this.downloadData,
      this.stores.profile.currentLocale
    );
  }

  @computed
  get downloadTimeLeft(): string {
    return this.formattedDownloadData.timeLeft;
  }

  @computed
  get totalDownloaded(): string {
    return this.formattedDownloadData.downloaded;
  }

  @computed
  get totalDownloadSize(): string {
    return this.formattedDownloadData.total;
  }

  @computed
  get downloadProgress(): number {
    return this.formattedDownloadData.progress;
  }

  @computed
  get showManualUpdate(): boolean {
    return this.isAutomaticUpdateFailed;
  }

  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  getUpdateInfo(update: News): SoftwareUpdateInfo {
    const softwareUpdate = get(update, 'softwareUpdate', {});
    const { version, hash, url } = softwareUpdate[platform] || {};
    return {
      version,
      hash,
      url,
    };
  }

  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  isUpdateInstalled = (update: News) => {
    const { version: updateVersion } = this.getUpdateInfo(update);
    return !semver.lt(currentVersion, updateVersion);
  };
  // =================== PRIVATE ==================
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  _checkNewAppUpdate = async (update: News) => {
    const { version, url } = this.getUpdateInfo(update);
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
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
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.setAppUpdateCompletedRequest.execute(version);
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
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
    // Cancels if the update download is already in progress
    if (this.isUpdateDownloading) return false;
    // Is there an 'Automatic Update Failed' flag?
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
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
      // The download is outdated
      if (info.fileUrl !== url) {
        await this._removeLocalDataInfo();
        return this._requestUpdateDownload(update);
      }

      // The user reopened Daedalus without installing the update
      if (data.state === DOWNLOAD_STATES.FINISHED && data.progress === 100) {
        // Does the file still exist?
        const installerFileStillExists = await this._checkFileExists();

        if (!installerFileStillExists) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
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
  _getUpdateDownloadLocalData = async (): Promise<
    DownloadLocalDataMainResponse
  > =>
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
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
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
    return Promise.resolve({
      fileUrl: '',
    });
  };
  _requestResumeUpdateDownload = async () => {
    await requestResumeDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ id: string; options: { progres... Remove this comment to see the full error message
      options: {
        progressIsThrottled: false,
        persistLocalData: true,
      },
    });
  };
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
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
      this.isWaitingToQuitDaedalus = true;
    });
    if (!this.availableUpdate) return false;
    const { destinationPath, originalFilename } = this.downloadInfo || {};
    const { hash } = this.getUpdateInfo(this.availableUpdate);
    const filePath = `${destinationPath}/${originalFilename}`;
    return manageAppUpdateChannel.request({
      filePath,
      hash,
    });
  };
  _manageQuitAndInstallResponse = ({
    status,
    data,
  }: ManageAppUpdateMainResponse) => {
    const { message, progress, error } = data;

    if (status === statuses.ERROR) {
      logger.error(message || '', {
        error,
      });
      runInAction(() => {
        this.isWaitingToQuitDaedalus = false;
      });

      this._setAppAutomaticUpdateFailed();
    } else if (status === statuses.PROGRESS) {
      if (progress) {
        runInAction(() => {
          this.installationProgress = progress;
        });
      }
    }

    return Promise.resolve({
      filePath: '',
      hash: '',
    });
  };
  _setAppAutomaticUpdateFailed = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setAppAutomaticUpdateFailedRequest.execute();
    runInAction(() => {
      this.isAutomaticUpdateFailed = true;
    });
  };
  @action
  _openAppUpdateOverlay = () => {
    this.isUpdateProgressOpen = true;
    this.isUpdatePostponed = false;
  };
  @action
  _closeAppUpdateOverlay = () => {
    this.isUpdateProgressOpen = false;
  };
  @action
  _postponeUpdate = () => {
    this.isUpdatePostponed = true;
  };
}
