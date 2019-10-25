// @flow
import { action, observable, computed, toJS, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment/moment';
import { includes } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { THEMES } from '../themes/index';
import { ROUTES } from '../routes-config';
import LocalizableError from '../i18n/LocalizableError';
import { WalletSupportRequestLogsCompressError } from '../i18n/errors';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { formattedBytesToSize } from '../utils/formatters';
import { Logger } from '../utils/logging';
import { setStateSnapshotLogChannel } from '../ipc/setStateSnapshotLogChannel';
import { detectSystemLocaleChannel } from '../ipc/detect-system-locale';
import { LOCALES } from '../../../common/types/locales.types';
import {
  compressLogsChannel,
  downloadLogsChannel,
  getLogsChannel,
} from '../ipc/logs.ipc';
import type { LogFiles, CompressedLogStatus } from '../types/LogTypes';
import type { StateSnapshotLogParams } from '../../../common/types/logging.types';
import {
  hasLoadedRequest,
  isRequestSet,
  requestGetter,
  getRequestKeys,
} from '../utils/storesUtils';
import {
  NUMBER_OPTIONS,
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  TIME_OPTIONS,
} from '../config/profileConfig';

// TODO: refactor all parts that rely on this to ipc channels!
const { ipcRenderer } = global;

export default class ProfileStore extends Store {
  @observable systemLocale: string = LOCALES.english;
  @observable systemNumberFormat: string = NUMBER_OPTIONS[0].value;
  @observable systemDateFormatEnglish: string = DATE_ENGLISH_OPTIONS[0].value;
  @observable systemDateFormatJapanese: string = DATE_JAPANESE_OPTIONS[0].value;
  @observable systemTimeFormat: string = TIME_OPTIONS[0].value;
  @observable bigNumberDecimalFormat = {
    decimalSeparator: '.',
    groupSeparator: ',',
    groupSize: 3,
    secondaryGroupSize: 0,
    fractionGroupSeparator: ' ',
    fractionGroupSize: 0,
  };

  /* eslint-disable max-len */
  @observable getProfileLocaleRequest: Request<string> = new Request(
    this.api.localStorage.getUserLocale
  );
  @observable setProfileLocaleRequest: Request<string> = new Request(
    this.api.localStorage.setUserLocale
  );
  @observable getProfileNumberFormatRequest: Request<string> = new Request(
    this.api.localStorage.getUserNumberFormat
  );
  @observable setProfileNumberFormatRequest: Request<string> = new Request(
    this.api.localStorage.setUserNumberFormat
  );
  @observable getProfileDateFormatEnglishRequest: Request<string> = new Request(
    this.api.localStorage.getUserDateFormatEnglish
  );
  @observable setProfileDateFormatEnglishRequest: Request<string> = new Request(
    this.api.localStorage.setUserDateFormatEnglish
  );
  @observable
  getProfileDateFormatJapaneseRequest: Request<string> = new Request(
    this.api.localStorage.getUserDateFormatJapanese
  );
  @observable
  setProfileDateFormatJapaneseRequest: Request<string> = new Request(
    this.api.localStorage.setUserDateFormatJapanese
  );
  @observable getProfileTimeFormatRequest: Request<string> = new Request(
    this.api.localStorage.getUserTimeFormat
  );
  @observable setProfileTimeFormatRequest: Request<string> = new Request(
    this.api.localStorage.setUserTimeFormat
  );
  @observable getTermsOfUseAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.getTermsOfUseAcceptance
  );
  @observable setTermsOfUseAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.setTermsOfUseAcceptance
  );
  @observable
  getDataLayerMigrationAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.getDataLayerMigrationAcceptance
  );
  @observable
  setDataLayerMigrationAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.setDataLayerMigrationAcceptance
  );
  @observable getThemeRequest: Request<string> = new Request(
    this.api.localStorage.getUserTheme
  );
  @observable setThemeRequest: Request<string> = new Request(
    this.api.localStorage.setUserTheme
  );
  @observable error: ?LocalizableError = null;
  @observable logFiles: LogFiles = {};
  @observable compressedLogsFilePath: ?string = null;
  @observable compressedLogsStatus: CompressedLogStatus = {};
  @observable isSubmittingBugReport: boolean = false;
  @observable isInitialScreen: boolean = false;
  /* eslint-enable max-len */

  setup() {
    this.actions.profile.finishInitialScreenSettings.listen(
      this._finishInitialScreenSettings
    );
    this.actions.profile.updateUserLocalSetting.listen(
      this._updateUserLocalSetting
    );
    this.actions.profile.updateUserLocalSettings.listen(
      this._updateUserLocalSettings
    );
    this.actions.profile.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    this.actions.profile.acceptDataLayerMigration.listen(
      this._acceptDataLayerMigration
    );
    this.actions.profile.updateTheme.listen(this._updateTheme);
    this.actions.profile.getLogs.listen(this._getLogs);
    this.actions.profile.getLogsAndCompress.listen(this._getLogsAndCompress);
    this.actions.profile.downloadLogs.listen(this._downloadLogs);
    this.actions.profile.downloadLogsSuccess.listen(
      this._toggleDisableDownloadLogs
    );
    this.actions.app.initAppEnvironment.listen(() => {});

    this.registerReactions([
      this._setBigNumberFormat,
      this._updateMomentJsLocaleAfterLocaleChange,
      this._reloadAboutWindowOnLocaleChange,
      this._redirectToInitialSettingsIfNoLocaleSet,
      this._redirectToTermsOfUseScreenIfTermsNotAccepted,
      this._redirectToDataLayerMigrationScreenIfMigrationHasNotAccepted,
      this._redirectToMainUiAfterTermsAreAccepted,
      this._redirectToMainUiAfterDataLayerMigrationIsAccepted,
    ]);
    this._getSystemLocale();
    this._getTermsOfUseAcceptance();
    this._getDataLayerMigrationAcceptance();
  }

  _setBigNumberFormat = () => {
    BigNumber.config({ FORMAT: this.bigNumberDecimalFormat });
  };

  @computed get currentLocale(): string {
    return requestGetter(this.getProfileLocaleRequest, this.systemLocale);
  }

  @computed get hasLoadedCurrentLocale(): boolean {
    return hasLoadedRequest(this.getProfileLocaleRequest);
  }

  @computed get isCurrentLocaleSet(): boolean {
    return isRequestSet(this.getProfileLocaleRequest);
  }

  @computed get currentTheme(): string {
    const systemValue = this.environment.isMainnet
      ? THEMES.DARK_BLUE
      : THEMES.LIGHT_BLUE; // defaults
    return requestGetter(this.getThemeRequest, systemValue);
  }

  @computed get isCurrentThemeSet(): boolean {
    return isRequestSet(this.getThemeRequest);
  }

  @computed get hasLoadedCurrentTheme(): boolean {
    return hasLoadedRequest(this.getThemeRequest);
  }

  @computed get currentNumberFormat(): string {
    return requestGetter(
      this.getProfileNumberFormatRequest,
      this.systemNumberFormat
    );
  }

  @computed get currentDateFormat(): string {
    return this.currentLocale === 'en-US'
      ? this.currentDateEnglishFormat
      : this.currentDateJapaneseFormat;
  }

  @computed get currentDateEnglishFormat(): string {
    return requestGetter(
      this.getProfileDateFormatEnglishRequest,
      this.systemDateFormatEnglish
    );
  }

  @computed get currentDateJapaneseFormat(): string {
    return requestGetter(
      this.getProfileDateFormatJapaneseRequest,
      this.systemDateFormatJapanese
    );
  }

  @computed get currentTimeFormat(): string {
    return requestGetter(
      this.getProfileTimeFormatRequest,
      this.systemTimeFormat
    );
  }

  @computed get termsOfUse(): string {
    const network = this.environment.isMainnet ? 'mainnet' : 'other';
    return require(`../i18n/locales/terms-of-use/${network}/${
      this.currentLocale
    }.md`);
  }

  @computed get hasLoadedTermsOfUseAcceptance(): boolean {
    return (
      this.getTermsOfUseAcceptanceRequest.wasExecuted &&
      this.getTermsOfUseAcceptanceRequest.result !== null
    );
  }

  @computed get areTermsOfUseAccepted(): boolean {
    return this.getTermsOfUseAcceptanceRequest.result === true;
  }

  @computed get hasLoadedDataLayerMigrationAcceptance(): boolean {
    return (
      this.getDataLayerMigrationAcceptanceRequest.wasExecuted &&
      this.getDataLayerMigrationAcceptanceRequest.result !== null
    );
  }

  @computed get isDataLayerMigrationAccepted(): boolean {
    return this.getDataLayerMigrationAcceptanceRequest.result === true;
  }

  @computed get isProfilePage(): boolean {
    const { currentRoute } = this.stores.app;
    return includes(ROUTES.PROFILE, currentRoute);
  }

  @computed get isSettingsPage(): boolean {
    const { currentRoute } = this.stores.app;
    return includes(ROUTES.SETTINGS, currentRoute);
  }

  _getSystemLocale = async () => {
    this._onReceiveSystemLocale(await detectSystemLocaleChannel.request());
  };

  _finishInitialScreenSettings = action(() => {
    this.isInitialScreen = false;
  });

  _updateUserLocalSetting = async ({
    param,
    value,
  }: {
    param: string,
    value: string,
  }) => {
    const { set, get } = getRequestKeys(param, this.currentLocale);
    await (this: any)[set].execute(value);
    await (this: any)[get].execute();
  };

  _updateUserLocalSettings = async (settings: { [key: string]: string }) => {
    Object.entries(settings).forEach(([param, value]) => {
      if (value && typeof value === 'string')
        this._updateUserLocalSetting({ param, value });
    });
  };

  _updateTheme = async ({ theme }: { theme: string }) => {
    await this.setThemeRequest.execute(theme);
    await this.getThemeRequest.execute();
  };

  _updateMomentJsLocaleAfterLocaleChange = () => {
    moment.locale(this.currentLocale);
  };

  _acceptTermsOfUse = async () => {
    await this.setTermsOfUseAcceptanceRequest.execute();
    await this.getTermsOfUseAcceptanceRequest.execute();
  };

  _getTermsOfUseAcceptance = () => {
    this.getTermsOfUseAcceptanceRequest.execute();
  };

  _acceptDataLayerMigration = async () => {
    await this.setDataLayerMigrationAcceptanceRequest.execute();
    await this.getDataLayerMigrationAcceptanceRequest.execute();
  };

  _getDataLayerMigrationAcceptance = () => {
    this.getDataLayerMigrationAcceptanceRequest.execute();
  };

  _redirectToInitialSettingsIfNoLocaleSet = () => {
    if (
      (this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) ||
      this.isInitialScreen
    ) {
      runInAction('Set `isInitialScreen` true', () => {
        this.isInitialScreen = true;
      });
      this.actions.router.goToRoute.trigger({
        route: ROUTES.PROFILE.INITIAL_SETTINGS,
      });
    }
  };

  _redirectToTermsOfUseScreenIfTermsNotAccepted = () => {
    const termsOfUseNotAccepted =
      this.hasLoadedTermsOfUseAcceptance && !this.areTermsOfUseAccepted;
    if (
      !this.isInitialScreen &&
      this.isCurrentLocaleSet &&
      termsOfUseNotAccepted
    ) {
      this.actions.router.goToRoute.trigger({
        route: ROUTES.PROFILE.TERMS_OF_USE,
      });
    }
  };

  _isOnTermsOfUsePage = () =>
    this.stores.app.currentRoute === ROUTES.PROFILE.TERMS_OF_USE;

  _redirectToDataLayerMigrationScreenIfMigrationHasNotAccepted = () => {
    const { isConnected } = this.stores.networkStatus;
    const dataLayerMigrationNotAccepted =
      this.hasLoadedDataLayerMigrationAcceptance &&
      !this.isDataLayerMigrationAccepted;
    if (
      isConnected &&
      this.isCurrentLocaleSet &&
      this.areTermsOfUseAccepted &&
      this.stores.wallets.hasLoadedWallets &&
      dataLayerMigrationNotAccepted
    ) {
      if (!this.stores.wallets.hasAnyWallets) {
        // There are no wallets to migrate so we just need
        // to set the data layer migration acceptance to true
        // in order to prevent future data migration checks
        this._acceptDataLayerMigration();
      } else {
        this.actions.router.goToRoute.trigger({
          route: ROUTES.PROFILE.DATA_LAYER_MIGRATION,
        });
      }
    }
  };

  _redirectToMainUiAfterTermsAreAccepted = () => {
    if (this.areTermsOfUseAccepted && this._isOnTermsOfUsePage()) {
      this._redirectToRoot();
    }
  };

  _redirectToMainUiAfterDataLayerMigrationIsAccepted = () => {
    if (
      this.isDataLayerMigrationAccepted &&
      this._isOnDataLayerMigrationPage()
    ) {
      this._redirectToRoot();
    }
  };

  _isOnDataLayerMigrationPage = () =>
    this.stores.app.currentRoute === ROUTES.PROFILE.DATA_LAYER_MIGRATION;

  _redirectToRoot = () => {
    this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

  _reloadAboutWindowOnLocaleChange = () => {
    // register mobx observer for currentLocale in order to trigger reaction on change
    this.currentLocale; // eslint-disable-line
    // TODO: refactor to ipc channel
    ipcRenderer.send('reload-about-window');
  };

  _setLogFiles = action((files: LogFiles) => {
    this.logFiles = files;
  });

  _getLogs = async () => {
    const { isDownloading } = this.compressedLogsStatus;
    await this._setStateSnapshotLog();
    const logs = await getLogsChannel.request();
    this._setLogFiles(logs);
    if (isDownloading || this.isSubmittingBugReport) {
      this._compressLogs({ logs });
    }
  };

  _compressLogs = action(async ({ logs }) => {
    const {
      fileName = generateFileNameWithTimestamp(),
    } = this.compressedLogsStatus;
    try {
      const outputPath = await compressLogsChannel.request({
        logs: toJS(logs),
        compressedFileName: fileName,
      });
      runInAction('ProfileStore::_compressLogs:success', () => {
        this.compressedLogsFilePath = outputPath;
        const { isDownloading, destination } = this.compressedLogsStatus;
        if (isDownloading) {
          this._downloadLogs({ destination, fileName });
        }
      });
    } catch (error) {
      runInAction('ProfileStore::_compressLogs:error', () => {
        this.isSubmittingBugReport = false;
        this.error = new WalletSupportRequestLogsCompressError();
      });
    }
  });

  _getLogsAndCompress = action(async () => {
    this.compressedLogsStatus = {
      fileName: generateFileNameWithTimestamp(),
    };
    this.isSubmittingBugReport = true;
    await this._getLogs();
  });

  _downloadLogs = action(async ({ fileName, destination, fresh }) => {
    this.compressedLogsStatus = {
      isDownloading: true,
      destination,
      fileName,
    };

    if (this.compressedLogsFilePath && fresh !== true) {
      // logs already compressed, trigger the download
      try {
        await downloadLogsChannel.request({
          compressedLogsFilePath: this.compressedLogsFilePath,
          destinationPath: destination,
        });
        this.actions.profile.downloadLogsSuccess.trigger(false);
        this._reset();
      } catch (error) {
        throw error;
      }
    } else {
      // start process: getLogs -> compressLogs -> downloadLogs (again)
      this._getLogs();
    }
  });

  // Collect all relevant state snapshot params and send them for log file creation
  _setStateSnapshotLog = async () => {
    try {
      Logger.info('ProfileStore: Requesting state snapshot log file creation');

      const { networkStatus } = this.stores;

      const {
        cardanoNodeID,
        tlsConfig,
        stateDirectoryPath,
        diskSpaceAvailable,
        cardanoNodeState,
        isConnected,
        forceCheckTimeDifferenceRequest,
        isNodeInSync,
        isNodeResponding,
        isNodeSubscribed,
        isNodeSyncing,
        isNodeTimeCorrect,
        isSynced,
        isSystemTimeCorrect,
        isSystemTimeIgnored,
        latestLocalBlockTimestamp,
        latestNetworkBlockTimestamp,
        localBlockHeight,
        localTimeDifference,
        networkBlockHeight,
        syncPercentage,
      } = networkStatus;

      const {
        network,
        buildNumber,
        cpu,
        version,
        mainProcessID,
        rendererProcessID,
        isBlankScreenFixActive,
        isDev,
        isMainnet,
        isStaging,
        isTestnet,
        os,
        platformVersion,
        ram,
      } = this.environment;

      const systemInfo = {
        platform: os,
        platformVersion,
        cpu: Array.isArray(cpu) ? cpu[0].model : '',
        ram: formattedBytesToSize(ram),
        availableDiskSpace: diskSpaceAvailable,
      };

      const coreInfo = {
        daedalusVersion: version,
        daedalusProcessID: rendererProcessID,
        daedalusMainProcessID: mainProcessID,
        isBlankScreenFixActive,
        cardanoVersion: buildNumber,
        cardanoProcessID: cardanoNodeID,
        cardanoAPIPort: tlsConfig ? tlsConfig.port : 0,
        cardanoNetwork: network,
        daedalusStateDirectoryPath: stateDirectoryPath,
      };

      const stateSnapshotData: StateSnapshotLogParams = {
        systemInfo,
        coreInfo,
        cardanoNodeState,
        currentLocale: this.currentLocale,
        isConnected,
        isDev,
        isForceCheckingNodeTime: forceCheckTimeDifferenceRequest.isExecuting,
        isMainnet,
        isNodeInSync,
        isNodeResponding,
        isNodeSubscribed,
        isNodeSyncing,
        isNodeTimeCorrect,
        isStaging,
        isSynced,
        isSystemTimeCorrect,
        isSystemTimeIgnored,
        isTestnet,
        latestLocalBlockTimestamp: moment(Date.now()).diff(
          moment(latestLocalBlockTimestamp)
        ),
        latestNetworkBlockTimestamp: moment(Date.now()).diff(
          moment(latestNetworkBlockTimestamp)
        ),
        localBlockHeight,
        localTimeDifference,
        networkBlockHeight,
        currentTime: new Date().toISOString(),
        syncPercentage: syncPercentage.toFixed(2),
      };

      await setStateSnapshotLogChannel.send(stateSnapshotData);
    } catch (error) {
      Logger.error('ProfileStore: State snapshot log file creation failed', {
        error,
      });
    }
  };

  _toggleDisableDownloadLogs = action(
    async ({ isDownloadNotificationVisible }) => {
      this.actions.app.setNotificationVisibility.trigger(
        isDownloadNotificationVisible
      );
    }
  );

  @action _onReceiveSystemLocale = (systemLocale: string) => {
    this.systemLocale = systemLocale;
  };

  @action _reset = () => {
    this.error = null;
    this.compressedLogsFilePath = null;
    this.compressedLogsStatus = {};
    this.isSubmittingBugReport = false;
  };
}
