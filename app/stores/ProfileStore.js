// @flow
import { action, observable, computed, toJS } from 'mobx';
import { concat, get, map } from 'lodash';
import BigNumber from 'bignumber.js';
import moment from 'moment/moment';
import { ipcRenderer } from 'electron';
import path from 'path';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import environment from '../environment';
import { THEMES } from '../themes/index';
import { ROUTES } from '../routes-config';
import { GET_LOGS } from '../../electron/ipc-api/get-logs';
import { COMPRESS_LOGS } from '../../electron/ipc-api/compress-logs';
import { DELETE_COMPRESSED_LOGS } from '../../electron/ipc-api/delete-compressed-logs';
import LocalizableError from '../i18n/LocalizableError';
import globalMessages from '../i18n/global-messages';
import { WalletSupportRequestLogsCompressError } from '../i18n/errors';
import type { LogFiles, CompressedLogs } from '../types/LogTypes';

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
  @observable getSendLogsChoiceRequest: Request<boolean> = new Request(this.api.localStorage.getSendLogsChoice);
  @observable setSendLogsChoiceRequest: Request = new Request(this.api.localStorage.setSendLogsChoice);
  @observable getThemeRequest: Request<string> = new Request(this.api.localStorage.getUserTheme);
  @observable setThemeRequest: Request<string> = new Request(this.api.localStorage.setUserTheme);
  @observable sendBugReport: Request<any> = new Request(this.api[environment.API].sendBugReport);
  @observable error: ?LocalizableError = null;
  @observable logFiles: LogFiles = {};
  @observable compressedLogs: CompressedLogs = {};
  @observable isCompressing: boolean = false;
  /* eslint-enable max-len */

  setup() {
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.actions.profile.setSendLogsChoice.listen(this._setSendLogsChoice);
    this.actions.profile.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    this.actions.profile.updateTheme.listen(this._updateTheme);
    this.actions.profile.getLogs.listen(this._getLogs);
    this.actions.profile.compressLogs.listen(this._compressLogs);
    this.actions.profile.sendBugReport.listen(this._sendBugReport);
    ipcRenderer.on(GET_LOGS.SUCCESS, this._onGetLogsSuccess);
    ipcRenderer.on(COMPRESS_LOGS.SUCCESS, this._onCompressLogsSuccess);
    ipcRenderer.on(COMPRESS_LOGS.ERROR, this._onCompressLogsError);
    this.registerReactions([
      this._setBigNumberFormat,
      this._updateMomentJsLocaleAfterLocaleChange,
      this._reloadAboutWindowOnLocaleChange,
      this._redirectToLanguageSelectionIfNoLocaleSet,
      this._redirectToTermsOfUseScreenIfTermsNotAccepted,
      this._redirectToSendLogsChoiceScreenIfSendLogsChoiceNotSet,
      this._redirectToMainUiAfterSetSendLogsChoice,
    ]);
    this._getTermsOfUseAcceptance();
    this._sendLogsChoiceToMainProcess();
  }

  teardown() {
    super.teardown();
    ipcRenderer.removeAllListeners(GET_LOGS.SUCCESS);
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

  @computed get isSendLogsChoiceSet(): boolean {
    return this.getSendLogsChoiceRequest.result !== null;
  }

  @computed get hasLoadedSendLogsChoice(): boolean {
    return this.getSendLogsChoiceRequest.wasExecuted;
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

  _getSendLogsChoice = async () => await this.getSendLogsChoiceRequest.execute().promise;

  _setSendLogsChoice = async ({ sendLogs }: { sendLogs: boolean }) => {
    await this.setSendLogsChoiceRequest.execute(sendLogs).promise;
    await this._sendLogsChoiceToMainProcess();
  };

  _sendLogsChoiceToMainProcess = async () => {
    const choice = await this._getSendLogsChoice();
    ipcRenderer.send('send-logs-choice', choice);
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

  _redirectToSendLogsChoiceScreenIfSendLogsChoiceNotSet = () => {
    const { isConnected } = this.stores.networkStatus;
    if (isConnected && this.isCurrentLocaleSet && this.areTermsOfUseAccepted &&
      this.hasLoadedSendLogsChoice && !this.isSendLogsChoiceSet) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.SEND_LOGS });
    }
  };

  _isOnSendLogsChoicePage = () => this.stores.app.currentRoute === ROUTES.PROFILE.SEND_LOGS;

  _redirectToMainUiAfterSetSendLogsChoice = () => {
    if (this.isSendLogsChoiceSet && this._isOnSendLogsChoicePage()) {
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

  _onGetLogsSuccess = action((event, res) => {
    this.logFiles = res;
  });

  _compressLogs = action(({ logs }) => {
    this.isCompressing = true;
    ipcRenderer.send(COMPRESS_LOGS.REQUEST, toJS(logs));
  });

  _onCompressLogsSuccess = action((event, res) => {
    this.isCompressing = false;
    this.compressedLogs = res;
  });

  _onCompressLogsError = action(() => {
    this.error = new WalletSupportRequestLogsCompressError();
  });

  _sendBugReport = action(({ email, subject, problem, logs } : {
    email: string,
    subject: ?string,
    problem: ?string,
    logs: ?Array<string>,
  }) => {
    this.sendBugReport.execute({
      email, subject, problem, logs,
    })
      .then(action(() => {
        this._deleteCompressedFiles(logs);
        this._reset();
        this.actions.dialogs.closeActiveDialog.trigger();
      }))
      .catch(action((error) => {
        this._deleteCompressedFiles(logs);
        this._reset();
        this.error = error;
      }));
  });

  _deleteCompressedFiles = action((logs) => {
    // Trigger ipc renderer to delete compressed temp files if exists
    if (logs) {
      let filesToDelete;
      const fileNames = logs.files;
      const compressedLogsOriginalFile = get(this.compressedLogs, 'originalFile');

      const files = [];
      map(fileNames, (fileName) => {
        const file = path.join(logs.path, fileName);
        files.push(file);
      });

      if (files.length > 1 && compressedLogsOriginalFile) {
        // if files are splitted then also include original file to delete
        filesToDelete = concat(files, compressedLogsOriginalFile);
      } else {
        filesToDelete = files;
      }
      ipcRenderer.send(DELETE_COMPRESSED_LOGS.REQUEST, filesToDelete);
    }
  })

  @action _reset = () => {
    this.error = null;
    this.compressedLogs = {};
  };

}
