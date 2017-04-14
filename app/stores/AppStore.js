// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import Request from './lib/Request';
import CachedRequest from './lib/CachedRequest';
import globalMessages from '../i18n/global-messages';
import LocalizableError from '../i18n/LocalizableError';
import { ROUTES } from '../Routes';
import { buildRoute } from '../lib/routing-helpers';

export default class AppStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: globalMessages.languageEnglish },
    { value: 'ja-JP', label: globalMessages.languageJapanese },
    { value: 'zh-CN', label: globalMessages.languageChinese },
    { value: 'ko-KR', label: globalMessages.languageKorean },
    { value: 'de-DE', label: globalMessages.languageGerman },
    { value: 'hr-HR', label: globalMessages.languageCroatian },
  ];

  @observable getProfileLocaleRequest: CachedRequest<string> = new CachedRequest(this.api.getUserLocale);
  @observable setProfileLocaleRequest: Request<string> = new Request(this.api.setUserLocale);
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
    if (this.isCurrentLocaleSet) {
      const result = this.getProfileLocaleRequest.execute().result;
      if (result != null) return result;
    }
    // default
    return 'en-US';
  }

  @computed get hasLoadedCurrentLocale(): boolean {
    return this.getProfileLocaleRequest.wasExecuted;
  }

  @computed get isCurrentLocaleSet(): boolean {
    return this.getProfileLocaleRequest.result != null;
  }

  _redirectToLanguageSelectionIfNoLocaleSet = () => {
    const { isConnected } = this.stores.networkStatus;
    if (isConnected && this.hasLoadedCurrentLocale && !this.isCurrentLocaleSet) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.PROFILE.LANGUAGE_SELECTION });
    }
  };

  _redirectToLoadingScreenWhenDisconnected = () => {
    if (!this.stores.networkStatus.isConnected) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
    }
  };

  @action _updateLocale = async ({ locale }: { locale: string }) => {
    await this.setProfileLocaleRequest.execute(locale);
    this.getProfileLocaleRequest.invalidate().patch(() => locale);
  };

  _updateRouteLocation = (options: { route: string, params: ?Object }) => {
    const routePath = buildRoute(options.route, options.params);
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== routePath) this.stores.router.push(routePath);
  };

  _redirectToMainUiAfterLocaleIsSet = () => {
    if (this.isCurrentLocaleSet && this.currentRoute === ROUTES.PROFILE.LANGUAGE_SELECTION) {
      this.actions.router.goToRoute.trigger({ route: ROUTES.ROOT });
    }
  };

}
