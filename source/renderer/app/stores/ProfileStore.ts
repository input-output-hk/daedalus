import {
  action,
  computed,
  makeObservable,
  observable,
  runInAction,
} from 'mobx';
import BigNumber from 'bignumber.js';
import { camelCase, includes } from 'lodash';
import { toJS } from '../../../common/utils/helper';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { THEMES } from '../themes/index';
import { ROUTES } from '../routes-config';
import LocalizableError from '../i18n/LocalizableError';
import { WalletSupportRequestLogsCompressError } from '../i18n/errors';
import { generateFileNameWithTimestamp } from '../../../common/utils/files';
import { logger } from '../utils/logging';
import { setStateSnapshotLogChannel } from '../ipc/setStateSnapshotLogChannel';
import { getDesktopDirectoryPathChannel } from '../ipc/getDesktopDirectoryPathChannel';
import { getSystemLocaleChannel } from '../ipc/getSystemLocaleChannel';
import type { Locale } from '../../../common/types/locales.types';
import { LOCALES } from '../../../common/types/locales.types';
import {
  compressLogsChannel,
  downloadLogsChannel,
  getLogsChannel,
} from '../ipc/logs.ipc';
import type { CompressedLogStatus, LogFiles } from '../types/LogTypes';
import type { StateSnapshotLogParams } from '../../../common/types/logging.types';
import {
  DEFAULT_NUMBER_FORMAT,
  NUMBER_FORMATS,
} from '../../../common/types/number.types';
import {
  getRequestKeys,
  hasLoadedRequest,
  isRequestSet,
  requestGetter,
  requestGetterLocale,
} from '../utils/storesUtils';
import {
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  NUMBER_OPTIONS,
  PROFILE_SETTINGS,
  TIME_OPTIONS,
} from '../config/profileConfig';
import { buildSystemInfo } from '../utils/buildSystemInfo';
import {
  AnalyticsAcceptanceStatus,
  AnalyticsTracker,
  EventCategories,
} from '../analytics/types';
import { Api } from '../api';
import { ActionsMap } from '../actions';

export default class ProfileStore extends Store {
  systemLocale: Locale = LOCALES.english;
  systemNumberFormat: string = NUMBER_OPTIONS[0].value;
  systemDateFormatEnglish: string = DATE_ENGLISH_OPTIONS[0].value;
  systemDateFormatJapanese: string = DATE_JAPANESE_OPTIONS[0].value;
  systemTimeFormat: string = TIME_OPTIONS[0].value;
  getProfileLocaleRequest: Request<string> = new Request(
    this.api.localStorage.getUserLocale
  );
  setProfileLocaleRequest: Request<string> = new Request(
    this.api.localStorage.setUserLocale
  );
  getProfileNumberFormatRequest: Request<string> = new Request(
    this.api.localStorage.getUserNumberFormat
  );
  setProfileNumberFormatRequest: Request<string> = new Request(
    this.api.localStorage.setUserNumberFormat
  );
  getProfileDateFormatEnglishRequest: Request<string> = new Request(
    this.api.localStorage.getUserDateFormatEnglish
  );
  setProfileDateFormatEnglishRequest: Request<string> = new Request(
    this.api.localStorage.setUserDateFormatEnglish
  );
  getProfileDateFormatJapaneseRequest: Request<string> = new Request(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'ProfileStor... Remove this comment to see the full error message
    this.api.localStorage.getUserDateFormatJapanese
  );
  setProfileDateFormatJapaneseRequest: Request<string> = new Request(
    // @ts-ignore ts-migrate(2339) FIXME: Property 'api' does not exist on type 'ProfileStor... Remove this comment to see the full error message
    this.api.localStorage.setUserDateFormatJapanese
  );
  getProfileTimeFormatRequest: Request<string> = new Request(
    this.api.localStorage.getUserTimeFormat
  );
  setProfileTimeFormatRequest: Request<string> = new Request(
    this.api.localStorage.setUserTimeFormat
  );
  getTermsOfUseAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.getTermsOfUseAcceptance
  );
  setTermsOfUseAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.setTermsOfUseAcceptance
  );
  getAnalyticsAcceptanceRequest: Request<AnalyticsAcceptanceStatus> =
    new Request(this.api.localStorage.getAnalyticsAcceptance);
  setAnalyticsAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.setAnalyticsAcceptance
  );
  getDataLayerMigrationAcceptanceRequest: Request<boolean> = new Request(
    this.api.localStorage.getDataLayerMigrationAcceptance
  );
  setDataLayerMigrationAcceptanceRequest: Request<string> = new Request(
    this.api.localStorage.setDataLayerMigrationAcceptance
  );
  getThemeRequest: Request<string> = new Request(
    this.api.localStorage.getUserTheme
  );
  setThemeRequest: Request<string> = new Request(
    this.api.localStorage.setUserTheme
  );
  error: LocalizableError | null | undefined = null;
  logFiles: LogFiles = null;
  compressedLogsFilePath: string | null | undefined = null;
  compressedLogsStatus: CompressedLogStatus = {};
  desktopDirectoryPath = '';
  isSubmittingBugReport = false;
  isInitialScreen = false;
  isRTSModeRecommendationAcknowledged = false;

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {
    super(api, actions, analytics);

    makeObservable(this, {
      systemLocale: observable,
      systemNumberFormat: observable,
      systemDateFormatEnglish: observable,
      systemDateFormatJapanese: observable,
      systemTimeFormat: observable,
      getProfileLocaleRequest: observable,
      setProfileLocaleRequest: observable,
      getProfileNumberFormatRequest: observable,
      setProfileNumberFormatRequest: observable,
      getProfileDateFormatEnglishRequest: observable,
      setProfileDateFormatEnglishRequest: observable,
      getProfileDateFormatJapaneseRequest: observable,
      setProfileDateFormatJapaneseRequest: observable,
      getProfileTimeFormatRequest: observable,
      setProfileTimeFormatRequest: observable,
      getTermsOfUseAcceptanceRequest: observable,
      setTermsOfUseAcceptanceRequest: observable,
      getAnalyticsAcceptanceRequest: observable,
      setAnalyticsAcceptanceRequest: observable,
      getDataLayerMigrationAcceptanceRequest: observable,
      setDataLayerMigrationAcceptanceRequest: observable,
      getThemeRequest: observable,
      setThemeRequest: observable,
      error: observable,
      logFiles: observable,
      compressedLogsFilePath: observable,
      compressedLogsStatus: observable,
      desktopDirectoryPath: observable,
      isSubmittingBugReport: observable,
      isInitialScreen: observable,
      isRTSModeRecommendationAcknowledged: observable,
      currentLocale: computed,
      hasLoadedCurrentLocale: computed,
      isCurrentLocaleSet: computed,
      currentTheme: computed,
      isCurrentThemeSet: computed,
      hasLoadedCurrentTheme: computed,
      currentNumberFormat: computed,
      currentDateFormat: computed,
      currentDateEnglishFormat: computed,
      currentDateJapaneseFormat: computed,
      currentTimeFormat: computed,
      currentTimeFormatShort: computed,
      termsOfUse: computed,
      hasLoadedTermsOfUseAcceptance: computed,
      areTermsOfUseAccepted: computed,
      analyticsAcceptanceStatus: computed,
      hasLoadedDataLayerMigrationAcceptance: computed,
      isDataLayerMigrationAccepted: computed,
      isProfilePage: computed,
      isSettingsPage: computed,
      _acknowledgeRTSFlagsModeRecommendation: action,
      _onReceiveSystemLocale: action,
      _onReceiveDesktopDirectoryPath: action,
      _reset: action,
    });
  }

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
      ...DEFAULT_NUMBER_FORMAT,
      ...NUMBER_FORMATS[this.currentNumberFormat],
    };
    BigNumber.config({
      FORMAT,
    });
  };

  get currentLocale(): Locale {
    return requestGetterLocale(this.getProfileLocaleRequest, this.systemLocale);
  }

  get hasLoadedCurrentLocale(): boolean {
    return hasLoadedRequest(this.getProfileLocaleRequest);
  }

  get isCurrentLocaleSet(): boolean {
    return isRequestSet(this.getProfileLocaleRequest);
  }

  get currentTheme(): string {
    // Default theme handling
    let systemValue;

    if (global.isFlight) {
      systemValue = THEMES.FLIGHT_CANDIDATE;
    } else {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'environment' does not exist on type 'Pro... Remove this comment to see the full error message
      systemValue = this.environment.isMainnet
        ? THEMES.DARK_CARDANO
        : THEMES.LIGHT_BLUE;
    }

    return requestGetter(this.getThemeRequest, systemValue);
  }

  get isCurrentThemeSet(): boolean {
    return isRequestSet(this.getThemeRequest);
  }

  get hasLoadedCurrentTheme(): boolean {
    return hasLoadedRequest(this.getThemeRequest);
  }

  get currentNumberFormat(): string {
    return requestGetter(
      this.getProfileNumberFormatRequest,
      this.systemNumberFormat
    );
  }

  get currentDateFormat(): string {
    return this.currentLocale === 'en-US'
      ? this.currentDateEnglishFormat
      : this.currentDateJapaneseFormat;
  }

  get currentDateEnglishFormat(): string {
    return requestGetter(
      this.getProfileDateFormatEnglishRequest,
      this.systemDateFormatEnglish
    );
  }

  get currentDateJapaneseFormat(): string {
    return requestGetter(
      this.getProfileDateFormatJapaneseRequest,
      this.systemDateFormatJapanese
    );
  }

  get currentTimeFormat(): string {
    return requestGetter(
      this.getProfileTimeFormatRequest,
      this.systemTimeFormat
    );
  }

  get currentTimeFormatShort(): string {
    return this.currentTimeFormat.replace(':ss', '');
  }

  get termsOfUse(): string {
    return require(`../i18n/locales/terms-of-use/${this.currentLocale}.md`)
      .default;
  }

  get hasLoadedTermsOfUseAcceptance(): boolean {
    return (
      this.getTermsOfUseAcceptanceRequest.wasExecuted &&
      this.getTermsOfUseAcceptanceRequest.result !== null
    );
  }

  get areTermsOfUseAccepted(): boolean {
    // @ts-ignore ts-migrate(2367) FIXME: This condition will always return 'false' since th... Remove this comment to see the full error message
    return this.getTermsOfUseAcceptanceRequest.result === true;
  }

  get analyticsAcceptanceStatus(): AnalyticsAcceptanceStatus {
    return this.getAnalyticsAcceptanceRequest.result;
  }

  get hasLoadedDataLayerMigrationAcceptance(): boolean {
    return (
      this.getDataLayerMigrationAcceptanceRequest.wasExecuted &&
      this.getDataLayerMigrationAcceptanceRequest.result !== null
    );
  }

  get isDataLayerMigrationAccepted(): boolean {
    return this.getDataLayerMigrationAcceptanceRequest.result === true;
  }

  get isProfilePage(): boolean {
    const { currentRoute } = this.stores.app;
    return includes(ROUTES.PROFILE, currentRoute);
  }

  get isSettingsPage(): boolean {
    const { currentRoute } = this.stores.app;
    return includes(ROUTES.SETTINGS, currentRoute);
  }

  _finishInitialScreenSettings = action(() => {
    this._consolidateUserSettings();

    this.isInitialScreen = false;
  });
  _consolidateUserSettings = () => {
    PROFILE_SETTINGS.forEach((param: string) => {
      this._updateUserLocalSetting({
        param,
      });
    });
  };
  _updateUserLocalSetting = async ({
    param,
    value,
  }: {
    param: string;
    value?: string;
  }) => {
    // In case `value` is missing, it consolidates in the localstorage the default value
    const consolidatedValue =
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string[]' is not assignable to p... Remove this comment to see the full error message
      value || (this as any)[camelCase(['current', param])];
    const { set, get } = getRequestKeys(param, this.currentLocale);
    await (this as any)[set].execute(consolidatedValue);
    await (this as any)[get].execute();

    if (param === 'numberFormat') {
      // Force re-rendering of the sidebar in order to apply new number format
      // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
      this.stores.wallets.refreshWalletsData();
    }

    this.analytics.sendEvent(
      EventCategories.SETTINGS,
      'Changed user settings',
      param
    );
  };
  _updateTheme = async ({ theme }: { theme: string }) => {
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.setThemeRequest.execute(theme);
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getThemeRequest.execute();

    this.analytics.sendEvent(EventCategories.SETTINGS, 'Changed theme', theme);
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
  _setAnalyticsAcceptanceStatus = (status: AnalyticsAcceptanceStatus) => {
    const previousStatus = this.analyticsAcceptanceStatus;

    this.setAnalyticsAcceptanceRequest.execute(status);
    this.getAnalyticsAcceptanceRequest.execute();

    if (status === AnalyticsAcceptanceStatus.ACCEPTED) {
      this.analytics.enableTracking();
    } else if (status === AnalyticsAcceptanceStatus.REJECTED) {
      this.analytics.disableTracking();
    }

    if (previousStatus === AnalyticsAcceptanceStatus.PENDING) {
      this._redirectToRoot();
    } else {
      this.actions.router.goToRoute.trigger({
        route: ROUTES.SETTINGS.SUPPORT,
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
      await getDesktopDirectoryPathChannel.request()
    );
  };
  _getSystemLocale = async () => {
    this._onReceiveSystemLocale(await getSystemLocaleChannel.request());
  };
  _redirectToInitialSettingsIfNoLocaleSet = () => {
    if (
      (this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) ||
      this.isInitialScreen
    ) {
      runInAction(() => {
        this.isInitialScreen = true;
      });
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
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
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
      this.actions.router.goToRoute.trigger({
        route: ROUTES.PROFILE.TERMS_OF_USE,
      });
    }
  };
  _isOnAnalyticsPage = () =>
    this.stores.app.currentRoute === ROUTES.PROFILE.ANALYTICS;
  _isOnTermsOfUsePage = () =>
    this.stores.app.currentRoute === ROUTES.PROFILE.TERMS_OF_USE;
  _redirectToAnalyticsScreenIfNotConfirmed = () => {
    if (
      !this.isInitialScreen &&
      this.isCurrentLocaleSet &&
      this.areTermsOfUseAccepted &&
      this.analyticsAcceptanceStatus === AnalyticsAcceptanceStatus.PENDING
    ) {
      this.actions.router.goToRoute.trigger({
        route: ROUTES.PROFILE.ANALYTICS,
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
      this.analyticsAcceptanceStatus !== AnalyticsAcceptanceStatus.PENDING &&
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
    // @ts-ignore ts-migrate(2339) FIXME: Property 'stores' does not exist on type 'ProfileS... Remove this comment to see the full error message
    this.stores.app.currentRoute === ROUTES.PROFILE.DATA_LAYER_MIGRATION;
  _redirectToRoot = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.ROOT,
    });
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
      this._compressLogs({
        logs,
      });
    }
  };
  _compressLogs = action(async ({ logs }) => {
    const { fileName = generateFileNameWithTimestamp() } =
      this.compressedLogsStatus;

    try {
      const outputPath = await compressLogsChannel.request({
        logs: toJS(logs),
        compressedFileName: fileName,
      });
      runInAction(() => {
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
      runInAction(() => {
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
  });
  // Collect all relevant state snapshot params and send them for log file creation
  _setStateSnapshotLog = async () => {
    try {
      logger.info('ProfileStore: Requesting state snapshot log file creation');
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
      const systemInfo = buildSystemInfo(this.environment, networkStatus);
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
      const stateSnapshotData: StateSnapshotLogParams = {
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
      await setStateSnapshotLogChannel.send(stateSnapshotData);
    } catch (error) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
      logger.error('ProfileStore: State snapshot log file creation failed', {
        error,
      });
    }
  };
  _toggleDisableDownloadLogs = action(
    async ({ isDownloadNotificationVisible }) => {
      // @ts-ignore ts-migrate(2339) FIXME: Property 'actions' does not exist on type 'Profile... Remove this comment to see the full error message
      this.actions.app.setIsDownloadingLogs.trigger(
        isDownloadNotificationVisible
      );
    }
  );
  _onReceiveSystemLocale = (systemLocale: Locale) => {
    this.systemLocale = systemLocale;
  };
  _onReceiveDesktopDirectoryPath = (desktopDirectoryPath: string) => {
    this.desktopDirectoryPath = desktopDirectoryPath;
  };
  _reset = () => {
    this.error = null;
    this.compressedLogsFilePath = null;
    this.compressedLogsStatus = {};
    this.isSubmittingBugReport = false;
  };
}
