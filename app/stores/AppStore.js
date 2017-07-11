// @flow
import { observable, computed } from 'mobx';
import { ipcRenderer } from 'electron';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import globalMessages from '../i18n/global-messages';
import LocalizableError from '../i18n/LocalizableError';
import { ROUTES } from '../Routes';
import { buildRoute } from '../lib/routing-helpers';

export default class AppStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: globalMessages.languageEnglish },
    { value: 'ja-JP', label: globalMessages.languageJapanese },
    // { value: 'zh-CN', label: globalMessages.languageChinese },
    // { value: 'ko-KR', label: globalMessages.languageKorean },
    // { value: 'de-DE', label: globalMessages.languageGerman },
    // { value: 'hr-HR', label: globalMessages.languageCroatian },
  ];

  /* eslint-disable max-len */
  @observable getProfileLocaleRequest: Request<string> = new Request(this.api.getUserLocale);
  @observable setProfileLocaleRequest: Request<string> = new Request(this.api.setUserLocale);
  @observable getTermsOfUseAcceptanceRequest: Request<string> = new Request(this.api.getTermsOfUseAcceptance);
  @observable setTermsOfUseAcceptanceRequest: Request<string> = new Request(this.api.setTermsOfUseAcceptance);
  @observable getSendLogsChoiceRequest: Request<string> = new Request(this.api.getSendLogsChoice);
  @observable setSendLogsChoiceRequest: Request<string> = new Request(this.api.setSendLogsChoice);
  @observable error: ?LocalizableError = null;
  /* eslint-enable max-len */

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.actions.profile.setSendLogsChoice.listen(this._setSendLogsChoice);
    this.actions.profile.acceptTermsOfUse.listen(this._acceptTermsOfUse);
    this.registerReactions([
      this._redirectToLanguageSelectionIfNoLocaleSet,
      this._redirectToTermsOfUseScreenIfTermsNotAccepted,
      this._redirectToSendLogsChoiceScreenIfSendLogsChoiceNotSet,
      this._redirectToMainUiAfterSetSendLogsChoice,
      this._redirectToLoadingScreenWhenDisconnected,
    ]);
    this._getTermsOfUseAcceptance();
    this._getSendLogsChoice();
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

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
    return (this.getProfileLocaleRequest.result != null && this.getProfileLocaleRequest.result !== '');
  }

  @computed get termsOfUse(): string {
    const localizedTermsOfUse = require(`../i18n/locales/terms-of-use/${this.currentLocale}.md`); // eslint-disable-line
    return localizedTermsOfUse;
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

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
  };

  _updateLocale = async ({ locale }: { locale: string }) => {
    await this.setProfileLocaleRequest.execute(locale);
    await this.getProfileLocaleRequest.execute();
  };

  _acceptTermsOfUse = async () => {
    await this.setTermsOfUseAcceptanceRequest.execute();
    await this.getTermsOfUseAcceptanceRequest.execute();
  };

  _getTermsOfUseAcceptance = () => {
    this.getTermsOfUseAcceptanceRequest.execute();
  };

  _getSendLogsChoice = async () => {
    const sendLogs = await this.getSendLogsChoiceRequest.execute().promise;
    ipcRenderer.send('send-logs-choice', sendLogs);
  };

  _setSendLogsChoice = async ({ sendLogs }: { sendLogs: boolean }) => {
    await this.setSendLogsChoiceRequest.execute(sendLogs);
    await this._getSendLogsChoice();
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

  _isOnSendLogsChoicePage = () => this.currentRoute === ROUTES.PROFILE.SEND_LOGS;

  _redirectToMainUiAfterSetSendLogsChoice = () => {
    if (this.isSendLogsChoiceSet && this._isOnSendLogsChoicePage()) {
      this._redirectToRoot();
    }
  };

  _redirectToLoadingScreenWhenDisconnected = () => {
    if (!this.stores.networkStatus.isConnected) {
      this._redirectToRoot();
    }
  };

  _redirectToRoot = () => {
    this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
  };

}
