// @flow
import { action, observable, computed, toJS } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment/moment';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import environment from '../../../common/environment';
import { THEMES } from '../themes/index';
import { ROUTES } from '../routes-config';
import { GET_LOGS, DOWNLOAD_LOGS, COMPRESS_LOGS, DELETE_COMPRESSED_LOGS } from '../../../common/ipc-api';
import LocalizableError from '../i18n/LocalizableError';
import globalMessages from '../i18n/global-messages';
import { WalletSupportRequestLogsCompressError } from '../i18n/errors';
import type { LogFiles, CompressedFileDownload } from '../types/LogTypes';

export default class SettingsStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: globalMessages.languageEnglish },
    { value: 'ja-JP', label: globalMessages.languageJapanese },
    // { value: 'zh-CN', label: globalMessages.languageChinese },
    // { value: 'ko-KR', label: globalMessages.languageKorean },
    // { value: 'de-DE', label: globalMessages.languageGerman },
    // { value: 'pl-PL', label: globalMessages.languagePolish },
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
  @observable getThemeRequest: Request<string> = new Request(this.api.localStorage.getUserTheme);
  @observable setThemeRequest: Request<string> = new Request(this.api.localStorage.setUserTheme);
  @observable sendBugReport: Request<any> = new Request(this.api[environment.API].sendBugReport);
  @observable error: ?LocalizableError = null;
  @observable logFiles: LogFiles = {};
  @observable compressedLog: ?string = null;
  @observable isCompressing: boolean = false;
  @observable compressedFileDownload: CompressedFileDownload = {};
  /* eslint-enable max-len */

  setup() {
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.actions.profile.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    this.actions.profile.updateTheme.listen(this._updateTheme);
    this.actions.profile.getLogs.listen(this._getLogs);
    this.actions.profile.resetBugReportDialog.listen(this._resetBugReportDialog);
    this.actions.profile.downloadLogs.listen(this._downloadLogs);
    this.actions.profile.compressLogs.listen(this._compressLogs);
    this.actions.profile.deleteCompressedLogs.listen(this._deleteCompressedFiles);
    this.actions.profile.sendBugReport.listen(this._sendBugReport);
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
      this._redirectToMainUiAfterTermsAreAccepted,
    ]);
    this._getTermsOfUseAcceptance();
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
      this.getProfileLocaleRequest.wasExecuted && this.getProfileLocaleRequest.result !== null
    );
  }

  @computed get isCurrentLocaleSet(): boolean {
    return (this.getProfileLocaleRequest.result !== null && this.getProfileLocaleRequest.result !== '');
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
    return (this.getThemeRequest.result !== null && this.getThemeRequest.result !== '');
  }

  @computed get hasLoadedCurrentTheme(): boolean {
    return (this.getThemeRequest.wasExecuted && this.getThemeRequest.result !== null);
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

  _redirectToLanguageSelectionIfNoLocaleSet = () => {
    const { isConnected } = this.stores.networkStatus;
    if (isConnected && this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.LANGUAGE_SELECTION });
    }
  };

  _redirectToTermsOfUseScreenIfTermsNotAccepted = () => {
    const { isConnected } = this.stores.networkStatus;
    if (isConnected && this.isCurrentLocaleSet &&
      this.hasLoadedTermsOfUseAcceptance && !this.areTermsOfUseAccepted) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.TERMS_OF_USE });
    }
  };

  _isOnTermsOfUsePage = () => this.stores.app.currentRoute === ROUTES.PROFILE.TERMS_OF_USE;

  _redirectToMainUiAfterTermsAreAccepted = () => {
    if (this.areTermsOfUseAccepted && this._isOnTermsOfUsePage()) {
      this._redirectToRoot();
    }
  };

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

  _resetBugReportDialog = () => {
    this._deleteCompressedFiles();
    this._reset();
    this.actions.dialogs.closeActiveDialog.trigger();
  };

  _downloadLogs = action(({ destination, fresh }) => {
    this.compressedFileDownload = {
      inProgress: true,
      destination,
    };

    if (this.compressedLog && fresh !== true) {
      // logs already compressed, trigger download
      ipcRenderer.send(DOWNLOAD_LOGS.REQUEST, this.compressedLog, destination);
    } else {
      // start process: getLogs -> compressLogs -> downloadLogs (again)
      this._getLogs();
    }
  });

  _onGetLogsSuccess = action((event, res) => {
    this.logFiles = res;
    if (this.compressedFileDownload.inProgress) {
      this._compressLogs({ logs: res });
    }
  });

  _onDownloadLogsSuccess = action(() => {
    this.compressedFileDownload = {};
  });

  _compressLogs = action(({ logs }) => {
    this.isCompressing = true;
    ipcRenderer.send(COMPRESS_LOGS.REQUEST, toJS(logs));
  });

  _onCompressLogsSuccess = action((event, res) => {
    this.isCompressing = false;
    this.compressedLog = res;
    if (this.compressedFileDownload.inProgress) {
      this._downloadLogs({ destination: this.compressedFileDownload.destination });
    }
  });

  _onCompressLogsError = action(() => {
    this.error = new WalletSupportRequestLogsCompressError();
  });

  _sendBugReport = action(({ email, subject, problem, compressedLog } : {
    email: string,
    subject: string,
    problem: string,
    compressedLog: ?string,
  }) => {
    this.sendBugReport.execute({
      email, subject, problem, compressedLog,
    })
      .then(action(() => {
        this._resetBugReportDialog();
      }))
      .catch(action((error) => {
        this.error = error;
      }));
  });

  _deleteCompressedFiles = action(() => {
    if (this.compressedLog) {
      ipcRenderer.send(DELETE_COMPRESSED_LOGS.REQUEST, this.compressedLog);
      this.compressedLog = null;
    }
  });

  @action _reset = () => {
    this.error = null;
    this.compressedLog = null;
    this.compressedFileDownload = {};
  };
}
