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
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const lodash_1 = require('lodash');
const helper_1 = require('../../../common/utils/helper');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const index_1 = require('../themes/index');
const routes_config_1 = require('../routes-config');
const LocalizableError_1 = __importDefault(require('../i18n/LocalizableError'));
const errors_1 = require('../i18n/errors');
const files_1 = require('../../../common/utils/files');
const logging_1 = require('../utils/logging');
const setStateSnapshotLogChannel_1 = require('../ipc/setStateSnapshotLogChannel');
const getDesktopDirectoryPathChannel_1 = require('../ipc/getDesktopDirectoryPathChannel');
const getSystemLocaleChannel_1 = require('../ipc/getSystemLocaleChannel');
const locales_types_1 = require('../../../common/types/locales.types');
const logs_ipc_1 = require('../ipc/logs.ipc');
const number_types_1 = require('../../../common/types/number.types');
const storesUtils_1 = require('../utils/storesUtils');
const profileConfig_1 = require('../config/profileConfig');
const buildSystemInfo_1 = require('../utils/buildSystemInfo');
const types_1 = require('../analytics/types');
class ProfileStore extends Store_1.default {
  systemLocale = locales_types_1.LOCALES.english;
  systemNumberFormat = profileConfig_1.NUMBER_OPTIONS[0].value;
  systemDateFormatEnglish = profileConfig_1.DATE_ENGLISH_OPTIONS[0].value;
  systemDateFormatJapanese = profileConfig_1.DATE_JAPANESE_OPTIONS[0].value;
  systemTimeFormat = profileConfig_1.TIME_OPTIONS[0].value;
  getProfileLocaleRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getUserLocale
  );
  setProfileLocaleRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setUserLocale
  );
  getProfileNumberFormatRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getUserNumberFormat
  );
  setProfileNumberFormatRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setUserNumberFormat
  );
  getProfileDateFormatEnglishRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getUserDateFormatEnglish
  );
  setProfileDateFormatEnglishRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setUserDateFormatEnglish
  );
  getProfileDateFormatJapaneseRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'ProfileStor... Remove this comment to see the full error message
    this.api.localStorage.getUserDateFormatJapanese
  );
  setProfileDateFormatJapaneseRequest = new LocalizedRequest_1.default(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'ProfileStor... Remove this comment to see the full error message
    this.api.localStorage.setUserDateFormatJapanese
  );
  getProfileTimeFormatRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getUserTimeFormat
  );
  setProfileTimeFormatRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setUserTimeFormat
  );
  getTermsOfUseAcceptanceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getTermsOfUseAcceptance
  );
  setTermsOfUseAcceptanceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setTermsOfUseAcceptance
  );
  getAnalyticsAcceptanceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getAnalyticsAcceptance
  );
  setAnalyticsAcceptanceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setAnalyticsAcceptance
  );
  getDataLayerMigrationAcceptanceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getDataLayerMigrationAcceptance
  );
  setDataLayerMigrationAcceptanceRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setDataLayerMigrationAcceptance
  );
  getThemeRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getUserTheme
  );
  setThemeRequest = new LocalizedRequest_1.default(
    this.api.localStorage.setUserTheme
  );
  error = null;
  logFiles = null;
  compressedLogsFilePath = null;
  compressedLogsStatus = {};
  desktopDirectoryPath = '';
  isSubmittingBugReport = false;
  isInitialScreen = false;
  isRTSModeRecommendationAcknowledged = false;
  /* eslint-enable max-len */
  setup() {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
    const { profile: profileActions } = this.actions;
    profileActions.finishInitialScreenSettings.listen(
      this._finishInitialScreenSettings
    );
    profileActions.updateUserLocalSetting.listen(this._updateUserLocalSetting);
    profileActions.acceptAnalytics.listen(this._setAnalyticsAcceptanceStatus);
    profileActions.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    profileActions.acceptDataLayerMigration.listen(
      this._acceptDataLayerMigration
    );
    profileActions.updateTheme.listen(this._updateTheme);
    profileActions.getLogs.listen(this._getLogs);
    profileActions.getLogsAndCompress.listen(this._getLogsAndCompress);
    profileActions.downloadLogs.listen(this._downloadLogs);
    profileActions.downloadLogsSuccess.listen(this._toggleDisableDownloadLogs);
    profileActions.acknowledgeRTSModeRecommendation.listen(
      this._acknowledgeRTSFlagsModeRecommendation
    );
    this.actions.app.initAppEnvironment.listen(() => {});
    this.registerReactions([
      this._updateBigNumberFormat,
      this._redirectToInitialSettingsIfNoLocaleSet,
      this._redirectToAnalyticsScreenIfNotConfirmed,
      this._redirectToTermsOfUseScreenIfTermsNotAccepted,
      this._redirectToMainUiAfterTermsAreAccepted,
      this._redirectToMainUiAfterDataLayerMigrationIsAccepted,
    ]);
    this._getTermsOfUseAcceptance();
    this._getAnalyticsAcceptance();
    this._getDataLayerMigrationAcceptance();
    this._getDesktopDirectoryPath();
    this._getSystemLocale();
  }
  _updateBigNumberFormat = () => {
    const FORMAT = {
      ...number_types_1.DEFAULT_NUMBER_FORMAT,
      ...number_types_1.NUMBER_FORMATS[this.currentNumberFormat],
    };
    bignumber_js_1.default.config({
      FORMAT,
    });
  };
  get currentLocale() {
    return (0, storesUtils_1.requestGetterLocale)(
      this.getProfileLocaleRequest,
      this.systemLocale
    );
  }
  get hasLoadedCurrentLocale() {
    return (0, storesUtils_1.hasLoadedRequest)(this.getProfileLocaleRequest);
  }
  get isCurrentLocaleSet() {
    return (0, storesUtils_1.isRequestSet)(this.getProfileLocaleRequest);
  }
  get currentTheme() {
    // Default theme handling
    let systemValue;
    if (global.isFlight) {
      systemValue = index_1.THEMES.FLIGHT_CANDIDATE;
    } else {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'Pro... Remove this comment to see the full error message
      systemValue = this.environment.isMainnet
        ? index_1.THEMES.DARK_CARDANO
        : index_1.THEMES.LIGHT_BLUE;
    }
    return (0, storesUtils_1.requestGetter)(this.getThemeRequest, systemValue);
  }
  get isCurrentThemeSet() {
    return (0, storesUtils_1.isRequestSet)(this.getThemeRequest);
  }
  get hasLoadedCurrentTheme() {
    return (0, storesUtils_1.hasLoadedRequest)(this.getThemeRequest);
  }
  get currentNumberFormat() {
    return (0, storesUtils_1.requestGetter)(
      this.getProfileNumberFormatRequest,
      this.systemNumberFormat
    );
  }
  get currentDateFormat() {
    return this.currentLocale === 'en-US'
      ? this.currentDateEnglishFormat
      : this.currentDateJapaneseFormat;
  }
  get currentDateEnglishFormat() {
    return (0, storesUtils_1.requestGetter)(
      this.getProfileDateFormatEnglishRequest,
      this.systemDateFormatEnglish
    );
  }
  get currentDateJapaneseFormat() {
    return (0, storesUtils_1.requestGetter)(
      this.getProfileDateFormatJapaneseRequest,
      this.systemDateFormatJapanese
    );
  }
  get currentTimeFormat() {
    return (0, storesUtils_1.requestGetter)(
      this.getProfileTimeFormatRequest,
      this.systemTimeFormat
    );
  }
  get currentTimeFormatShort() {
    return this.currentTimeFormat.replace(':ss', '');
  }
  get termsOfUse() {
    return require(`../i18n/locales/terms-of-use/${this.currentLocale}.md`)
      .default;
  }
  get hasLoadedTermsOfUseAcceptance() {
    return (
      this.getTermsOfUseAcceptanceRequest.wasExecuted &&
      this.getTermsOfUseAcceptanceRequest.result !== null
    );
  }
  get areTermsOfUseAccepted() {
    // @ts-ignore ts-migrate(2367) FIXME: This condition will always return 'false' since th... Remove this comment to see the full error message
    return this.getTermsOfUseAcceptanceRequest.result === true;
  }
  get analyticsAcceptanceStatus() {
    return this.getAnalyticsAcceptanceRequest.result;
  }
  get hasLoadedDataLayerMigrationAcceptance() {
    return (
      this.getDataLayerMigrationAcceptanceRequest.wasExecuted &&
      this.getDataLayerMigrationAcceptanceRequest.result !== null
    );
  }
  get isDataLayerMigrationAccepted() {
    return this.getDataLayerMigrationAcceptanceRequest.result === true;
  }
  get isProfilePage() {
    const { currentRoute } = this.stores.app;
    return (0, lodash_1.includes)(routes_config_1.ROUTES.PROFILE, currentRoute);
  }
  get isSettingsPage() {
    const { currentRoute } = this.stores.app;
    return (0, lodash_1.includes)(
      routes_config_1.ROUTES.SETTINGS,
      currentRoute
    );
  }
  _finishInitialScreenSettings = (0, mobx_1.action)(() => {
    this._consolidateUserSettings();
    this.isInitialScreen = false;
  });
  _consolidateUserSettings = () => {
    profileConfig_1.PROFILE_SETTINGS.forEach((param) => {
      this._updateUserLocalSetting({
        param,
      });
    });
  };
  _updateUserLocalSetting = async ({ param, value }) => {
    // In case `value` is missing, it consolidates in the localstorage the default value
    const consolidatedValue =
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
      value || this[(0, lodash_1.camelCase)(['current', param])];
    const { set, get } = (0, storesUtils_1.getRequestKeys)(
      param,
      this.currentLocale
    );
    await this[set].execute(consolidatedValue);
    await this[get].execute();
    if (param === 'numberFormat') {
      // Force re-rendering of the sidebar in order to apply new number format
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
      this.stores.wallets.refreshWalletsData();
    }
    this.analytics.sendEvent(
      types_1.EventCategories.SETTINGS,
      'Changed user settings',
      param
    );
  };
  _updateTheme = async ({ theme }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setThemeRequest.execute(theme);
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getThemeRequest.execute();
    this.analytics.sendEvent(
      types_1.EventCategories.SETTINGS,
      'Changed theme',
      theme
    );
  };
  _acceptTermsOfUse = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setTermsOfUseAcceptanceRequest.execute();
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getTermsOfUseAcceptanceRequest.execute();
  };
  _getTermsOfUseAcceptance = () => {
    this.getTermsOfUseAcceptanceRequest.execute();
  };
  _setAnalyticsAcceptanceStatus = (status) => {
    const previousStatus = this.analyticsAcceptanceStatus;
    this.setAnalyticsAcceptanceRequest.execute(status);
    this.getAnalyticsAcceptanceRequest.execute();
    if (status === types_1.AnalyticsAcceptanceStatus.ACCEPTED) {
      this.analytics.enableTracking();
    } else if (status === types_1.AnalyticsAcceptanceStatus.REJECTED) {
      this.analytics.disableTracking();
    }
    if (previousStatus === types_1.AnalyticsAcceptanceStatus.PENDING) {
      this._redirectToRoot();
    } else {
      this.actions.router.goToRoute.trigger({
        route: routes_config_1.ROUTES.SETTINGS.SUPPORT,
      });
    }
  };
  _getAnalyticsAcceptance = () => {
    this.getAnalyticsAcceptanceRequest.execute();
  };
  _acceptDataLayerMigration = async () => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setDataLayerMigrationAcceptanceRequest.execute();
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getDataLayerMigrationAcceptanceRequest.execute();
  };
  _acknowledgeRTSFlagsModeRecommendation = () => {
    this.isRTSModeRecommendationAcknowledged = true;
  };
  _getDataLayerMigrationAcceptance = () => {
    this.getDataLayerMigrationAcceptanceRequest.execute();
  };
  _getDesktopDirectoryPath = async () => {
    this._onReceiveDesktopDirectoryPath(
      await getDesktopDirectoryPathChannel_1.getDesktopDirectoryPathChannel.request()
    );
  };
  _getSystemLocale = async () => {
    this._onReceiveSystemLocale(
      await getSystemLocaleChannel_1.getSystemLocaleChannel.request()
    );
  };
  _redirectToInitialSettingsIfNoLocaleSet = () => {
    if (
      (this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) ||
      this.isInitialScreen
    ) {
      (0, mobx_1.runInAction)('Set `isInitialScreen` true', () => {
        this.isInitialScreen = true;
      });
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
      this.actions.router.goToRoute.trigger({
        route: routes_config_1.ROUTES.PROFILE.INITIAL_SETTINGS,
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
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
      this.actions.router.goToRoute.trigger({
        route: routes_config_1.ROUTES.PROFILE.TERMS_OF_USE,
      });
    }
  };
  _isOnAnalyticsPage = () =>
    this.stores.app.currentRoute === routes_config_1.ROUTES.PROFILE.ANALYTICS;
  _isOnTermsOfUsePage = () =>
    this.stores.app.currentRoute ===
    routes_config_1.ROUTES.PROFILE.TERMS_OF_USE;
  _redirectToAnalyticsScreenIfNotConfirmed = () => {
    if (
      !this.isInitialScreen &&
      this.isCurrentLocaleSet &&
      this.areTermsOfUseAccepted &&
      this.analyticsAcceptanceStatus ===
        types_1.AnalyticsAcceptanceStatus.PENDING
    ) {
      this.actions.router.goToRoute.trigger({
        route: routes_config_1.ROUTES.PROFILE.ANALYTICS,
      });
    }
  };
  _redirectToDataLayerMigrationScreenIfMigrationHasNotAccepted = () => {
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
    const { isConnected } = this.stores.networkStatus;
    const dataLayerMigrationNotAccepted =
      this.hasLoadedDataLayerMigrationAcceptance &&
      !this.isDataLayerMigrationAccepted;
    if (
      isConnected &&
      this.isCurrentLocaleSet &&
      this.areTermsOfUseAccepted &&
      this.analyticsAcceptanceStatus !==
        types_1.AnalyticsAcceptanceStatus.PENDING &&
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
      this.stores.wallets.hasLoadedWallets &&
      dataLayerMigrationNotAccepted
    ) {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
      if (!this.stores.wallets.hasAnyWallets) {
        // There are no wallets to migrate:
        // set the data layer migration acceptance to true
        // in order to prevent future data migration checks
        this._acceptDataLayerMigration();
      } else {
        // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
        this.actions.router.goToRoute.trigger({
          route: routes_config_1.ROUTES.PROFILE.DATA_LAYER_MIGRATION,
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
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
    this.stores.app.currentRoute ===
    routes_config_1.ROUTES.PROFILE.DATA_LAYER_MIGRATION;
  _redirectToRoot = () => {
    this.actions.router.goToRoute.trigger({
      route: routes_config_1.ROUTES.ROOT,
    });
  };
  _setLogFiles = (0, mobx_1.action)((files) => {
    this.logFiles = files;
  });
  _getLogs = async () => {
    const { isDownloading } = this.compressedLogsStatus;
    await this._setStateSnapshotLog();
    const logs = await logs_ipc_1.getLogsChannel.request();
    this._setLogFiles(logs);
    if (isDownloading || this.isSubmittingBugReport) {
      this._compressLogs({
        logs,
      });
    }
  };
  _compressLogs = (0, mobx_1.action)(async ({ logs }) => {
    const {
      fileName = (0, files_1.generateFileNameWithTimestamp)(),
    } = this.compressedLogsStatus;
    try {
      const outputPath = await logs_ipc_1.compressLogsChannel.request({
        logs: (0, helper_1.toJS)(logs),
        compressedFileName: fileName,
      });
      (0, mobx_1.runInAction)('ProfileStore::_compressLogs:success', () => {
        this.compressedLogsFilePath = outputPath;
        const { isDownloading, destination } = this.compressedLogsStatus;
        if (isDownloading) {
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ destination: string; fileName:... Remove this comment to see the full error message
          this._downloadLogs({
            destination,
            fileName,
          });
        }
      });
    } catch (error) {
      (0, mobx_1.runInAction)('ProfileStore::_compressLogs:error', () => {
        this.isSubmittingBugReport = false;
        this.error = new errors_1.WalletSupportRequestLogsCompressError();
      });
    }
  });
  _getLogsAndCompress = (0, mobx_1.action)(async () => {
    this.compressedLogsStatus = {
      fileName: (0, files_1.generateFileNameWithTimestamp)(),
    };
    this.isSubmittingBugReport = true;
    await this._getLogs();
  });
  _downloadLogs = (0, mobx_1.action)(
    async ({ fileName, destination, fresh }) => {
      this.compressedLogsStatus = {
        isDownloading: true,
        destination,
        fileName,
      };
      if (this.compressedLogsFilePath && fresh !== true) {
        // logs already compressed, trigger the download
        try {
          await logs_ipc_1.downloadLogsChannel.request({
            compressedLogsFilePath: this.compressedLogsFilePath,
            destinationPath: destination,
          });
          // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
          this.actions.profile.downloadLogsSuccess.trigger(false);
          this._reset();
        } catch (error) {
          throw error;
        }
      } else {
        // start process: getLogs -> compressLogs -> downloadLogs (again)
        this._getLogs();
      }
    }
  );
  // Collect all relevant state snapshot params and send them for log file creation
  _setStateSnapshotLog = async () => {
    try {
      logging_1.logger.info(
        'ProfileStore: Requesting state snapshot log file creation'
      );
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
      const { networkStatus } = this.stores;
      const {
        cardanoNodePID,
        cardanoWalletPID,
        tlsConfig,
        stateDirectoryPath,
        cardanoNodeState,
        isConnected,
        isNodeInSync,
        isNodeResponding,
        isNodeSyncing,
        isSynced,
        syncPercentage,
        localTip,
        networkTip,
      } = networkStatus;
      const {
        build,
        network,
        apiVersion,
        nodeVersion,
        version,
        mainProcessID,
        rendererProcessID,
        isBlankScreenFixActive,
        isDev,
        isMainnet,
        isStaging,
        isTestnet,
      } = this.environment;
      const systemInfo = (0, buildSystemInfo_1.buildSystemInfo)(
        this.environment,
        networkStatus
      );
      const coreInfo = {
        daedalusVersion: version,
        daedalusBuildNumber: build,
        daedalusProcessID: rendererProcessID,
        daedalusMainProcessID: mainProcessID,
        isBlankScreenFixActive,
        cardanoNodeVersion: nodeVersion,
        cardanoNodePID,
        cardanoWalletVersion: apiVersion,
        cardanoWalletPID,
        cardanoWalletApiPort: tlsConfig ? tlsConfig.port : 0,
        cardanoNetwork: network,
        daedalusStateDirectoryPath: stateDirectoryPath,
      };
      const stateSnapshotData = {
        systemInfo,
        coreInfo,
        cardanoNodeState,
        currentLocale: this.currentLocale,
        isConnected,
        isDev,
        isMainnet,
        isNodeInSync,
        isNodeResponding,
        isNodeSyncing,
        isStaging,
        isSynced,
        isTestnet,
        currentTime: new Date().toISOString(),
        syncPercentage: syncPercentage.toFixed(2),
        localTip: localTip
          ? {
              epoch: localTip.epoch,
              slot: localTip.slot,
            }
          : localTip,
        networkTip: networkTip
          ? {
              epoch: networkTip.epoch,
              slot: networkTip.slot,
            }
          : networkTip,
      };
      await setStateSnapshotLogChannel_1.setStateSnapshotLogChannel.send(
        stateSnapshotData
      );
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logging_1.logger.error(
        'ProfileStore: State snapshot log file creation failed',
        {
          error,
        }
      );
    }
  };
  _toggleDisableDownloadLogs = (0, mobx_1.action)(
    async ({ isDownloadNotificationVisible }) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
      this.actions.app.setIsDownloadingLogs.trigger(
        isDownloadNotificationVisible
      );
    }
  );
  _onReceiveSystemLocale = (systemLocale) => {
    this.systemLocale = systemLocale;
  };
  _onReceiveDesktopDirectoryPath = (desktopDirectoryPath) => {
    this.desktopDirectoryPath = desktopDirectoryPath;
  };
  _reset = () => {
    this.error = null;
    this.compressedLogsFilePath = null;
    this.compressedLogsStatus = {};
    this.isSubmittingBugReport = false;
  };
}
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  ProfileStore.prototype,
  'systemLocale',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  ProfileStore.prototype,
  'systemNumberFormat',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  ProfileStore.prototype,
  'systemDateFormatEnglish',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  ProfileStore.prototype,
  'systemDateFormatJapanese',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  ProfileStore.prototype,
  'systemTimeFormat',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getProfileLocaleRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setProfileLocaleRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getProfileNumberFormatRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setProfileNumberFormatRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getProfileDateFormatEnglishRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setProfileDateFormatEnglishRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getProfileDateFormatJapaneseRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setProfileDateFormatJapaneseRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getProfileTimeFormatRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setProfileTimeFormatRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getTermsOfUseAcceptanceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setTermsOfUseAcceptanceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getAnalyticsAcceptanceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setAnalyticsAcceptanceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getDataLayerMigrationAcceptanceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setDataLayerMigrationAcceptanceRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'getThemeRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  ProfileStore.prototype,
  'setThemeRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizableError_1.default)],
  ProfileStore.prototype,
  'error',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ProfileStore.prototype,
  'logFiles',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  ProfileStore.prototype,
  'compressedLogsFilePath',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ProfileStore.prototype,
  'compressedLogsStatus',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ProfileStore.prototype,
  'desktopDirectoryPath',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ProfileStore.prototype,
  'isSubmittingBugReport',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ProfileStore.prototype,
  'isInitialScreen',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  ProfileStore.prototype,
  'isRTSModeRecommendationAcknowledged',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentLocale',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'hasLoadedCurrentLocale',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'isCurrentLocaleSet',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentTheme',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'isCurrentThemeSet',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'hasLoadedCurrentTheme',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentNumberFormat',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentDateFormat',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentDateEnglishFormat',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentDateJapaneseFormat',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentTimeFormat',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'currentTimeFormatShort',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'termsOfUse',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'hasLoadedTermsOfUseAcceptance',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'areTermsOfUseAccepted',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', String),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'analyticsAcceptanceStatus',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'hasLoadedDataLayerMigrationAcceptance',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'isDataLayerMigrationAccepted',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'isProfilePage',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Boolean),
    __metadata('design:paramtypes', []),
  ],
  ProfileStore.prototype,
  'isSettingsPage',
  null
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  ProfileStore.prototype,
  '_acknowledgeRTSFlagsModeRecommendation',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  ProfileStore.prototype,
  '_onReceiveSystemLocale',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  ProfileStore.prototype,
  '_onReceiveDesktopDirectoryPath',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  ProfileStore.prototype,
  '_reset',
  void 0
);
exports.default = ProfileStore;
//# sourceMappingURL=ProfileStore.js.map
