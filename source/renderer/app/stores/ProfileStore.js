// @flow
import { action, observable, computed, toJS } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment/moment';
import { ipcRenderer } from 'electron';
import { includes } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import environment from '../../../common/environment';
import { THEMES } from '../themes/index';
import { ROUTES } from '../routes-config';
import { GET_LOGS, DOWNLOAD_LOGS, COMPRESS_LOGS } from '../../../common/ipc-api';
import LocalizableError from '../i18n/LocalizableError';
import globalMessages from '../i18n/global-messages';
import { WalletSupportRequestLogsCompressError } from '../i18n/errors';
import type { LogFiles, CompressedLogStatus } from '../types/LogTypes';
import { generateFileNameWithTimestamp } from '../../../common/fileName';

export default class SettingsStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: globalMessages.languageEnglish },
    { value: 'ja-JP', label: globalMessages.languageJapanese },
    // { value: 'zh-CN', label: globalMessages.languageChinese },
    // { value: 'ko-KR', label: globalMessages.languageKorean },
    // { value: 'de-DE', label: globalMessages.languageGerman },
    // { value: 'hr-HR', label: globalMessages.languageCroatian },
  ];

  @observable bigNumberDecimalFormat = {
    decimalSeparator: '.',
    groupSeparator: ',',
    groupSize: 3,
    secondaryGroupSize: 0,
    fractionGroupSeparator: ' ',
    fractionGroupSize: 0
  };

  /* eslint-disable max-len */
  @observable getProfileLocaleRequest: Request<string> = new Request(this.api.localStorage.getUserLocale);
  @observable setProfileLocaleRequest: Request<string> = new Request(this.api.localStorage.setUserLocale);
  @observable getTermsOfUseAcceptanceRequest: Request<string> = new Request(this.api.localStorage.getTermsOfUseAcceptance);
  @observable setTermsOfUseAcceptanceRequest: Request<string> = new Request(this.api.localStorage.setTermsOfUseAcceptance);
  @observable getDataLayerMigrationAcceptanceRequest: Request<string> = new Request(this.api.localStorage.getDataLayerMigrationAcceptance);
  @observable setDataLayerMigrationAcceptanceRequest: Request<string> = new Request(this.api.localStorage.setDataLayerMigrationAcceptance);
  @observable getThemeRequest: Request<string> = new Request(this.api.localStorage.getUserTheme);
  @observable setThemeRequest: Request<string> = new Request(this.api.localStorage.setUserTheme);
  @observable sendBugReport: Request<any> = new Request(this.api[environment.API].sendBugReport);
  @observable error: ?LocalizableError = null;
  @observable logFiles: LogFiles = {};
  @observable compressedLogsFile: ?string = null;
  @observable compressedLogsStatus: CompressedLogStatus = {};
  @observable isSubmittingBugReport: boolean = false;
  /* eslint-enable max-len */

  setup() {
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.actions.profile.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    this.actions.profile.acceptDataLayerMigration.listen(this._acceptDataLayerMigration);
    this.actions.profile.updateTheme.listen(this._updateTheme);
    this.actions.profile.getLogs.listen(this._getLogs);
    this.actions.profile.getLogsAndCompress.listen(this._getLogsAndCompress);
    this.actions.profile.sendBugReport.listen(this._sendBugReport);
    this.actions.profile.resetBugReportDialog.listen(this._resetBugReportDialog);
    this.actions.profile.downloadLogs.listen(this._downloadLogs);
    ipcRenderer.on(GET_LOGS.SUCCESS, this._onGetLogsSuccess);
    ipcRenderer.on(DOWNLOAD_LOGS.SUCCESS, this._onDownloadLogsSuccess);
    ipcRenderer.on(COMPRESS_LOGS.SUCCESS, this._onCompressLogsSuccess);
    ipcRenderer.on(COMPRESS_LOGS.ERROR, this._onCompressLogsError);
    this.registerReactions([
      this._setBigNumberFormat,
      this._updateMomentJsLocaleAfterLocaleChange,
      this._reloadAboutWindowOnLocaleChange,
      this._redirectToLanguageSelectionIfNoLocaleSet,
      this._redirectToTermsOfUseScreenIfTermsNotAccepted,
      this._redirectToDataLayerMigrationScreenIfMigrationHasNotAccepted,
      this._redirectToMainUiAfterTermsAreAccepted,
      this._redirectToMainUiAfterDataLayerMigrationIsAccepted,
    ]);
    this._getTermsOfUseAcceptance();
    this._getDataLayerMigrationAcceptance();
  }

  teardown() {
    super.teardown();
    ipcRenderer.removeAllListeners(GET_LOGS.SUCCESS);
    ipcRenderer.removeAllListeners(DOWNLOAD_LOGS.SUCCESS);
    ipcRenderer.removeAllListeners(COMPRESS_LOGS.SUCCESS);
    ipcRenderer.removeAllListeners(COMPRESS_LOGS.ERROR);
  }

  _setBigNumberFormat = () => {
    BigNumber.config({ FORMAT: this.bigNumberDecimalFormat });
  };

  @computed get currentLocale(): string {
    const { result } = this.getProfileLocaleRequest.execute();
    if (this.isCurrentLocaleSet) return result;
    return 'en-US'; // default
  }

  @computed get hasLoadedCurrentLocale(): boolean {
    return (
      this.getProfileLocaleRequest.wasExecuted &&
      this.getProfileLocaleRequest.result !== null
    );
  }

  @computed get isCurrentLocaleSet(): boolean {
    return (
      this.getProfileLocaleRequest.result !== null &&
      this.getProfileLocaleRequest.result !== ''
    );
  }

  @computed get currentTheme(): string {
    const { result } = this.getThemeRequest.execute();
    if (this.isCurrentThemeSet) return result;
    if (environment.isAdaApi()) {
      return environment.isMainnet() ? THEMES.DARK_BLUE : THEMES.LIGHT_BLUE; // defaults
    }
    return THEMES.LIGHT_BLUE; // default for ETC
  }

  @computed get isCurrentThemeSet(): boolean {
    return (
      this.getThemeRequest.result !== null &&
      this.getThemeRequest.result !== ''
    );
  }

  @computed get hasLoadedCurrentTheme(): boolean {
    return (
      this.getThemeRequest.wasExecuted &&
      this.getThemeRequest.result !== null
    );
  }

  @computed get termsOfUse(): string {
    const network = environment.isMainnet() ? 'mainnet' : 'other';
    return require(`../i18n/locales/terms-of-use/${environment.API}/${network}/${this.currentLocale}.md`);
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

  @computed get hasDataLayerMigrationAccepted(): boolean {
    return this.getDataLayerMigrationAcceptanceRequest.result === true;
  }

  @computed get isSettingsPage(): boolean {
    const { currentRoute } = this.stores.app;
    return (
      includes(ROUTES.PROFILE, currentRoute) ||
      includes(ROUTES.SETTINGS, currentRoute)
    );
  }

  _updateLocale = async ({ locale }: { locale: string }) => {
    await this.setProfileLocaleRequest.execute(locale);
    await this.getProfileLocaleRequest.execute();
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

  _skipDataLayerMigration = async () => {
    await this.setDataLayerMigrationAcceptanceRequest.execute();
    await this.getDataLayerMigrationAcceptanceRequest.execute();
  };

  _acceptDataLayerMigration = async () => {
    await this.setDataLayerMigrationAcceptanceRequest.execute();
    await this.getDataLayerMigrationAcceptanceRequest.execute();
  };

  _getDataLayerMigrationAcceptance = () => {
    this.getDataLayerMigrationAcceptanceRequest.execute();
  };

  _redirectToLanguageSelectionIfNoLocaleSet = () => {
    const { isConnected } = this.stores.networkStatus;
    if (isConnected && this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.LANGUAGE_SELECTION });
    }
  };

  _redirectToTermsOfUseScreenIfTermsNotAccepted = () => {
    const { isConnected } = this.stores.networkStatus;
    const termsOfUseNotAccepted = this.hasLoadedTermsOfUseAcceptance && !this.areTermsOfUseAccepted;
    if (isConnected && this.isCurrentLocaleSet && termsOfUseNotAccepted) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.TERMS_OF_USE });
    }
  };

  _isOnTermsOfUsePage = () => this.stores.app.currentRoute === ROUTES.PROFILE.TERMS_OF_USE;

  _redirectToDataLayerMigrationScreenIfMigrationHasNotAccepted = () => {
    const { isConnected } = this.stores.networkStatus;
    const dataLayerMigrationNotAccepted =
      this.hasLoadedDataLayerMigrationAcceptance && !this.hasDataLayerMigrationAccepted;
    if (
      isConnected &&
      this.isCurrentLocaleSet &&
      this.areTermsOfUseAccepted &&
      this.stores.ada.wallets.hasLoadedWallets &&
      dataLayerMigrationNotAccepted
    ) {
      if (!this.stores.ada.wallets.hasAnyWallets) {
        this._skipDataLayerMigration();
      } else {
        this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.DATA_LAYER_MIGRATION });
      }
    }
  };

  _redirectToMainUiAfterTermsAreAccepted = () => {
    if (this.areTermsOfUseAccepted && this._isOnTermsOfUsePage()) {
      this._redirectToRoot();
    }
  };

  _redirectToMainUiAfterDataLayerMigrationIsAccepted = () => {
    if (this.hasDataLayerMigrationAccepted && this._isOnDataLayerMigrationPage()) {
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
    ipcRenderer.send('reload-about-window');
  };

  _getLogs = () => {
    ipcRenderer.send(GET_LOGS.REQUEST);
  };

  _onGetLogsSuccess = action((event, files) => {
    this.logFiles = files;
    const { isDownloading } = this.compressedLogsStatus;
    if (isDownloading || this.isSubmittingBugReport) {
      this._compressLogs({ logs: files });
    }
  });

  _getLogsAndCompress = action(() => {
    this.compressedLogsStatus = {
      fileName: generateFileNameWithTimestamp(),
    };
    this.isSubmittingBugReport = true;
    this._getLogs();
  });

  _compressLogs = action(({ logs }) => {
    const { fileName = generateFileNameWithTimestamp() } = this.compressedLogsStatus;
    ipcRenderer.send(COMPRESS_LOGS.REQUEST, toJS(logs), fileName);
  });

  _onCompressLogsSuccess = action((event, file) => {
    this.compressedLogsFile = file;
    const { isDownloading, destination, fileName } = this.compressedLogsStatus;
    if (isDownloading) {
      this._downloadLogs({ destination, fileName });
    }
  });

  _onCompressLogsError = action(() => {
    this.isSubmittingBugReport = false;
    this.error = new WalletSupportRequestLogsCompressError();
  });

  _sendBugReport = action(({ email, subject, problem, compressedLogsFile } : {
    email: string,
    subject: string,
    problem: string,
    compressedLogsFile: ?string,
  }) => {
    this.isSubmittingBugReport = true;
    this.sendBugReport.execute({
      email, subject, problem, compressedLogsFile,
    })
      .then(action(() => {
        this._resetBugReportDialog();
      }))
      .catch(action((error) => {
        this.isSubmittingBugReport = false;
        this.error = error;
      }));
  });

  _resetBugReportDialog = () => {
    this._reset();
    this.actions.dialogs.closeActiveDialog.trigger();
  };

  _downloadLogs = action(({ fileName, destination, fresh }) => {
    this.compressedLogsStatus = {
      isDownloading: true,
      destination,
      fileName,
    };
    if (this.compressedLogsFile && fresh !== true) {
      // logs already compressed, trigger the download
      ipcRenderer.send(DOWNLOAD_LOGS.REQUEST, this.compressedLogsFile, destination);
    } else {
      // start process: getLogs -> compressLogs -> downloadLogs (again)
      this._getLogs();
    }
  });

  _onDownloadLogsSuccess = action(() => {
    this.compressedLogsStatus = {};
  });

  @action _reset = () => {
    this.error = null;
    this.compressedLogsFile = null;
    this.compressedLogsStatus = {};
    this.isSubmittingBugReport = false;
  };
}
