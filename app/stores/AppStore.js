// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';
import globalMessages from '../i18n/global-messages';
import LocalizableError from '../i18n/LocalizableError';

export default class AppStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: globalMessages.languageEnglish },
    { value: 'ja-JP', label: globalMessages.languageJapanese },
    { value: 'zh-CN', label: globalMessages.languageChinese },
    { value: 'ko-KR', label: globalMessages.languageKorean },
    { value: 'de-DE', label: globalMessages.languageGerman },
    { value: 'hr-HR', label: globalMessages.languageCroatian },
  ];

  @observable getProfileLocaleRequest = new CachedRequest(this.api, 'getUserLocale');
  @observable setProfileLocaleRequest = new Request(this.api, 'setUserLocale');
  @observable error: ?LocalizableError = null;

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.registerReactions([
      this._redirectToMainUiAfterLocaleIsSet,
      this._redirectToLanguageSelectionIfNoLocaleSet,
      this._redirectToLoadingScreenWhenDisconnected
    ]);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get currentLocale(): string {
    if (!this.isCurrentLocaleSet) {
      this.getProfileLocaleRequest.execute();
      return 'en-US';
    }
    return this.getProfileLocaleRequest.result;
  }

  @computed get hasLoadedCurrentLocale(): bool {
    return this.getProfileLocaleRequest.wasExecuted;
  }

  @computed get isCurrentLocaleSet(): bool {
    return this.getProfileLocaleRequest.result;
  }

  _redirectToLanguageSelectionIfNoLocaleSet = () => {
    const { isConnected } = this.stores.networkStatus;
    if (isConnected && this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) {
      this.actions.router.goToRoute({ route: '/profile/language-selection' });
    }
  };

  _redirectToLoadingScreenWhenDisconnected = () => {
    if (!this.stores.networkStatus.isConnected) {
      this.actions.router.goToRoute({ route: '/' });
    }
  };

  @action _updateLocale = async ({ locale }: { locale: string }) => {
    await this.setProfileLocaleRequest.execute(locale);
    this.getProfileLocaleRequest.invalidate().patch(() => locale);
  };

  _updateRouteLocation = ({ route }: { route: string }) => {
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== route) this.stores.router.push(route);
  };

  _redirectToMainUiAfterLocaleIsSet = () => {
    if (this.isCurrentLocaleSet && this.currentRoute === '/profile/language-selection') {
      this.actions.router.goToRoute({ route: '/' });
    }
  };

}
