// @flow
import { observable, computed } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment/moment';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import environment from '../environment';
import { THEMES } from '../themes/index';
import { ROUTES } from '../routes-config';
import globalMessages from '../i18n/global-messages';

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
  /* eslint-enable max-len */

  setup() {
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.actions.profile.setSendLogsChoice.listen(this._setSendLogsChoice);
    this.actions.profile.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    this.actions.profile.updateTheme.listen(this._updateTheme);
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
    return environment.isMainnet() ? THEMES.DARK_BLUE : THEMES.LIGHT_BLUE; // default
  }

  @computed get isCurrentThemeSet(): boolean {
    return (this.getThemeRequest.result !== null && this.getThemeRequest.result !== '');
  }

  @computed get hasLoadedCurrentTheme(): boolean {
    return (this.getThemeRequest.wasExecuted && this.getThemeRequest.result !== null);
  }

  @computed get termsOfUse(): string {
    const network = environment.isMainnet() ? 'mainnet' : 'other';
    return require(`../i18n/locales/terms-of-use/${network}/${this.currentLocale}.md`);
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
}
