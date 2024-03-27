'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const semver_1 = __importDefault(require('semver'));
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const logging_1 = require('../utils/logging');
const downloadManagerChannel_1 = require('../ipc/downloadManagerChannel');
const manageAppUpdateChannel_1 = require('../ipc/manageAppUpdateChannel');
const appUpdateConfig_1 = require('../../../common/config/appUpdateConfig');
const formatters_1 = require('../utils/formatters');
const downloadManagerConfig_1 = require('../../../common/config/downloadManagerConfig');
const { version: currentVersion, platform } = global.environment;
class AppUpdateStore extends Store_1.default {
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  availableUpdate = null;
  availableUpdateVersion = '';
  isUpdateDownloading = false;
  isUpdateDownloaded = false;
  isUpdateProgressOpen = false;
  isAutomaticUpdateFailed = false;
  isUpdatePostponed = false;
  isWaitingToQuitDaedalus = false;
  installationProgress = 0;
  downloadInfo = null;
  downloadData = null;
  availableAppVersion = null;
  getAppAutomaticUpdateFailedRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getAppAutomaticUpdateFailed
  );
  setAppAutomaticUpdateFailedRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setAppAutomaticUpdateFailed
  );
  unsetAppAutomaticUpdateFailedRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetAppAutomaticUpdateFailed
  );
  getAppUpdateCompletedRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getAppUpdateCompleted
  );
  setAppUpdateCompletedRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setAppUpdateCompleted
  );
  unsetAppUpdateCompletedRequest = new LocalizedRequest_1.default(
    this.api.localStorage.unsetAppUpdateCompleted
  );
  setup() {
    const actions = this.actions.appUpdate;
    actions.installUpdate.listen(this._installUpdate);
    actions.openAppUpdateOverlay.listen(this._openAppUpdateOverlay);
    actions.closeAppUpdateOverlay.listen(this._closeAppUpdateOverlay);
    actions.postponeUpdate.listen(this._postponeUpdate);
    downloadManagerChannel_1.requestDownloadChannel.onReceive(
      this._manageUpdateResponse
    );
    manageAppUpdateChannel_1.manageAppUpdateChannel.onReceive(
      this._manageQuitAndInstallResponse
    );
    // ============== MOBX REACTIONS ==============
    this.registerReactions([this._watchForNewsfeedUpdates]);
  }
  // ================= REACTIONS ==================
  _watchForNewsfeedUpdates = async () => {
    const { update } = this.stores.newsFeed.newsFeedData;
    if (update) {
      this._checkNewAppUpdate(update);
    } else {
      const isFileInDownloads = await this._checkFileExists();
      if (isFileInDownloads && !this.availableUpdate) {
        this._clearDownloads();
      }
    }
  };
  _clearDownloads = async () => {
    this._removeUpdateFile();
    await this._removeLocalDataInfo();
  };
  // ==================== PUBLIC ==================
  get displayAppUpdateOverlay() {
    return (
      !!this.availableUpdate &&
      !this.isUpdatePostponed &&
      (this.isUpdateProgressOpen ||
        this.isUpdateDownloaded ||
        this.isAutomaticUpdateFailed)
    );
  }
  get displayAppUpdateNewsItem() {
    return this.isUpdateDownloading || this.isUpdatePostponed;
  }
  get formattedDownloadData() {
    return (0, formatters_1.formattedDownloadData)(
      this.downloadData,
      this.stores.profile.currentLocale
    );
  }
  get downloadTimeLeft() {
    return this.formattedDownloadData.timeLeft;
  }
  get totalDownloaded() {
    return this.formattedDownloadData.downloaded;
  }
  get totalDownloadSize() {
    return this.formattedDownloadData.total;
  }
  get downloadProgress() {
    return this.formattedDownloadData.progress;
  }
  get showManualUpdate() {
    return this.isAutomaticUpdateFailed;
  }
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  getUpdateInfo(update) {
    const softwareUpdate = (0, lodash_1.get)(update, 'softwareUpdate', {});
    const { version, hash, url } = softwareUpdate[platform] || {};
    return {
      version,
      hash,
      url,
    };
  }
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  isUpdateInstalled = (update) => {
    const { version: updateVersion } = this.getUpdateInfo(update);
    return !semver_1.default.lt(currentVersion, updateVersion);
  };
  // =================== PRIVATE ==================
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  _checkNewAppUpdate = async (update) => {
    const { version, url } = this.getUpdateInfo(update);
    const appUpdateCompleted =
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.getAppUpdateCompletedRequest.execute();
    /*
     * The update was already installed and the installer was already deleted.
     * We can't simply compare with the `package.json` version
     * otherwise we would trigger the local data cleaning on every app load
     */
    if (appUpdateCompleted === version) {
      return false;
    }
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
    (0, mobx_1.runInAction)(() => {
      this.availableUpdate = update;
      this.availableUpdateVersion = version;
    });
    // Cancels if the update download is already in progress
    if (this.isUpdateDownloading) return false;
    // Is there an 'Automatic Update Failed' flag?
    const isAutomaticUpdateFailed =
      // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
      await this.getAppAutomaticUpdateFailedRequest.execute();
    if (isAutomaticUpdateFailed) {
      (0, mobx_1.runInAction)(() => {
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
      if (
        data.state === downloadManagerConfig_1.DOWNLOAD_STATES.FINISHED &&
        data.progress === 100
      ) {
        // Does the file still exist?
        const installerFileStillExists = await this._checkFileExists();
        if (!installerFileStillExists) {
          // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
          logging_1.logger.error(
            'AppUpdateStore:_setAppAutomaticUpdateFailed: Failed to find the installer file'
          );
          this._setAppAutomaticUpdateFailed();
          return false;
        }
        (0, mobx_1.runInAction)(() => {
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
    downloadManagerChannel_1.clearDownloadLocalDataChannel.request({
      id: appUpdateConfig_1.APP_UPDATE_DOWNLOAD_ID,
    });
  };
  _removeUpdateFile = () => {
    downloadManagerChannel_1.deleteDownloadedFile.request({
      id: appUpdateConfig_1.APP_UPDATE_DOWNLOAD_ID,
    });
  };
  _getUpdateDownloadLocalData = async () =>
    downloadManagerChannel_1.getDownloadLocalDataChannel.request({
      id: appUpdateConfig_1.APP_UPDATE_DOWNLOAD_ID,
    });
  _checkFileExists = async () =>
    downloadManagerChannel_1.checkFileExistsChannel.request({
      id: appUpdateConfig_1.APP_UPDATE_DOWNLOAD_ID,
    });
  _manageUpdateResponse = ({ eventType, info, data, error }) => {
    (0, mobx_1.runInAction)('updates the download information', () => {
      if (eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.PAUSE) {
        this.availableUpdate = null;
        this.isUpdateDownloaded = true;
      }
      if (eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.PROGRESS) {
        this.downloadInfo = info;
        this.downloadData = data;
      }
      if (eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.END) {
        this.isUpdateDownloaded = true;
        // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
        this.actions.app.closeNewsFeed.trigger();
      }
      if (eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.ERROR) {
        logging_1.logger.error(
          'AppUpdateStore:_setAppAutomaticUpdateFailed: Received an error event from the main process',
          {
            error,
          }
        );
        this._setAppAutomaticUpdateFailed();
      }
      if (
        eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.END ||
        eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.PAUSE ||
        eventType === downloadManagerConfig_1.DOWNLOAD_EVENT_TYPES.ERROR
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
    await downloadManagerChannel_1.requestResumeDownloadChannel.request({
      id: appUpdateConfig_1.APP_UPDATE_DOWNLOAD_ID,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ id: string; options: { progress... Remove this comment to see the full error message
      options: {
        progressIsThrottled: false,
        persistLocalData: true,
      },
    });
  };
  // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
  _requestUpdateDownload = (update) => {
    const { url: fileUrl } = this.getUpdateInfo(update);
    if (!fileUrl) return null;
    return downloadManagerChannel_1.requestDownloadChannel.request({
      id: appUpdateConfig_1.APP_UPDATE_DOWNLOAD_ID,
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
      logging_1.logger.error(
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
    (0, mobx_1.runInAction)(() => {
      this.isWaitingToQuitDaedalus = true;
    });
    if (!this.availableUpdate) return false;
    const { destinationPath, originalFilename } = this.downloadInfo || {};
    const { hash } = this.getUpdateInfo(this.availableUpdate);
    const filePath = `${destinationPath}/${originalFilename}`;
    return manageAppUpdateChannel_1.manageAppUpdateChannel.request({
      filePath,
      hash,
    });
  };
  _manageQuitAndInstallResponse = ({ status, data }) => {
    const { message, progress, error } = data;
    if (status === appUpdateConfig_1.UPDATE_INSTALLATION_STATUSES.ERROR) {
      logging_1.logger.error(message || '', {
        error,
      });
      (0, mobx_1.runInAction)(() => {
        this.isWaitingToQuitDaedalus = false;
      });
      this._setAppAutomaticUpdateFailed();
    } else if (
      status === appUpdateConfig_1.UPDATE_INSTALLATION_STATUSES.PROGRESS
    ) {
      if (progress) {
        (0, mobx_1.runInAction)(() => {
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
    (0, mobx_1.runInAction)(() => {
      this.isAutomaticUpdateFailed = true;
    });
  };
  _openAppUpdateOverlay = () => {
    this.isUpdateProgressOpen = true;
    this.isUpdatePostponed = false;
  };
  _closeAppUpdateOverlay = () => {
    this.isUpdateProgressOpen = false;
  };
  _postponeUpdate = () => {
    this.isUpdatePostponed = true;
  };
}
__decorate(
  [
    mobx_1.observable,
    // @ts-ignore ts-migrate(2749) FIXME: 'News' refers to a value, but is being used as a t... Remove this comment to see the full error message
    __metadata('design:type', Object),
  ],
  AppUpdateStore.prototype,
  'availableUpdate',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'availableUpdateVersion',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'isUpdateDownloading',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'isUpdateDownloaded',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'isUpdateProgressOpen',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'isAutomaticUpdateFailed',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'isUpdatePostponed',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'isWaitingToQuitDaedalus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'installationProgress',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'downloadInfo',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  'downloadData',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  AppUpdateStore.prototype,
  'availableAppVersion',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AppUpdateStore.prototype,
  'getAppAutomaticUpdateFailedRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AppUpdateStore.prototype,
  'setAppAutomaticUpdateFailedRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AppUpdateStore.prototype,
  'unsetAppAutomaticUpdateFailedRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AppUpdateStore.prototype,
  'getAppUpdateCompletedRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AppUpdateStore.prototype,
  'setAppUpdateCompletedRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  AppUpdateStore.prototype,
  'unsetAppUpdateCompletedRequest',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'displayAppUpdateOverlay',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'displayAppUpdateNewsItem',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'formattedDownloadData',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'downloadTimeLeft',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'totalDownloaded',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'totalDownloadSize',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Number),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'downloadProgress',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  AppUpdateStore.prototype,
  'showManualUpdate',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  '_openAppUpdateOverlay',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  '_closeAppUpdateOverlay',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  AppUpdateStore.prototype,
  '_postponeUpdate',
  void 0
);
exports.default = AppUpdateStore;
//# sourceMappingURL=AppUpdateStore.js.map
