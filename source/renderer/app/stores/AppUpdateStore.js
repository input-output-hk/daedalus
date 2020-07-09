// @flow
import { action, computed, observable, runInAction } from 'mobx';
import { get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import type { GetLatestAppVersionResponse } from '../api/nodes/types';
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

const { News } = NewsDomains;

const APP_UPDATE_DOWNLOAD_ID = 'appUpdate';

export default class AppUpdateStore extends Store {
  @observable availableUpdate: ?News = null;

  @observable isDownloadingUpdate: boolean = false;

  @observable isUpdateAvailable: boolean = false;

  // @observable isDownloadingUpdate: boolean = true;

  @observable isUpdatePostponed: boolean = false;
  @observable isUpdateInstalled: boolean = false;
  @observable hasPendingDownload: boolean = false;

  @observable availableAppVersion: ?string = null;
  @observable isNewAppVersionAvailable: boolean = false;
  @observable applicationVersion: ?number = null;
  @observable downloadProgress: ?number = 45;
  // @observable downloadProgress: ?number = null;
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

    // this._init();
  }

  // ================= REACTIONS ==================

  _watchForNewsfeedUpdates = () => {
    const { update } = this.stores.newsFeed.newsFeedData;
    if (update) this._checkNewAppUpdate(update);
  };

  // =============== PRIVATE ===============

  _checkNewAppUpdate = async (update: News) => {
    // Is the update valid?
    const isValidUpdate = await this._isUpdateValid(update);
    if (!isValidUpdate) {
      this._removeLocalDataInfo();
      return;
    }

    runInAction(() => {
      this.availableUpdate = update;
    });

    // Is there a pending / resumable download?
    const unfinishedDownload = await this._getUpdateDownloadLocalData();
    if (unfinishedDownload) {
      if (this._isUnfinishedDownloadValid(unfinishedDownload)) {
        this._requestResumeUpdateDownload();
        return;
      }
    }

    await this._removeLocalDataInfo();
    this._requestUpdateDownload(update);
  };

  _isUpdateValid = async (update: News) => {
    // eslint-disable-next-line
    console.log('update', update);
    return true;
  };

  _isUnfinishedDownloadValid = async (
    unfinishedDownload: DownloadsLocalDataMainResponse
  ) => {
    // eslint-disable-next-line
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
    progress,
  }: DownloadMainResponse) => {
    if (eventType === 'progress') {
      // eslint-disable-next-line
      console.log(
        '%c Download progress: %s%',
        'color: darkOrange',
        parseInt(progress.progress, 10)
      );
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
    });
  };

  _requestUpdateDownload = async (update: News) => {
    console.log('update', update);
    const fileUrl = get(update, 'download.darwin');
    if (!fileUrl) return;
    await requestDownloadChannel.request({
      id: APP_UPDATE_DOWNLOAD_ID,
      fileUrl,
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
