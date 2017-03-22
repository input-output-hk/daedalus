// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class AppStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: 'English' },
    { value: 'ja-JP', label: 'Japanese' },
    { value: 'zh-CN', label: 'Chinese' },
    { value: 'ko-KR', label: 'Korean' },
    { value: 'de-DE', label: 'German' },
    { value: 'hr-HR', label: 'Croatian' },
  ];

  @observable profileLocaleRequest = new CachedRequest(this.api, 'setUserLocale');

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this.registerReactions([
      this._redirectToMainUiAfterLocaleIsSet,
      this._redirectToLanguageSelectionIfNoLocaleSet
    ]);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get currentLocale(): string {
    if (!this.isCurrentLocaleSet) {
      return 'en-US';
    }
    return this.profileLocaleRequest.result;
  }

  @computed get isCurrentLocaleSet(): bool {
    return this.profileLocaleRequest.wasExecuted;
  }

  _redirectToLanguageSelectionIfNoLocaleSet = () => {
    if (!this.isCurrentLocaleSet) {
      this.actions.router.goToRoute({ route: '/profile/language-selection' });
    }
  };

  @action _updateLocale = ({ locale }: { locale: string }) => {
    this.profileLocaleRequest.execute(locale);
  };

  _updateRouteLocation = ({ route }: { route: string }) => {
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== route) this.stores.router.push(route);
  };

  _redirectToMainUiAfterLocaleIsSet = () => {
    if (this.isCurrentLocaleSet && this.stores.app.currentRoute === '/profile/language-selection') {
      this.actions.router.goToRoute({ route: '/' });
    }
  };

}
