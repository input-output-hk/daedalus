// @flow
import { observable, action, computed } from 'mobx';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

export default class AppStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: 'English' },
    { value: 'ja_JP', label: 'Japanese' },
    { value: 'zh-CN', label: 'Chinese' },
    { value: 'ko_KR', label: 'Korean' },
    { value: 'de-DE', label: 'German' },
    { value: 'hr-HR', label: 'Croatian' },
  ];

  @observable profileLocaleRequest = new CachedRequest(this.api, 'setUserLocale');

  setup() {
    this.actions.router.goToRoute.listen(this._updateRouteLocation);
    this.actions.profile.updateLocale.listen(this._updateLocale);
    this._redirectToLanguageSelectionIfNoLocaleSet();
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get currentLocale(): string {
    if (!this.profileLocaleRequest.wasExecuted) {
      return 'en-US';
    }
    return this.profileLocaleRequest.result;
  }

  @computed get isCurrentLocaleSet(): string {
    return this.profileLocaleRequest.wasExecuted;
  }

  @action _redirectToLanguageSelectionIfNoLocaleSet = () => {
    if (!this.profileLocaleRequest.wasExecuted)  {
      this._updateRouteLocation({ route: '/profile/language-selection' });
    }
  };

  @action _updateLocale = ({ locale }: { locale: string }) => {
    this.profileLocaleRequest.execute(locale);
    this._updateRouteLocation({ route: '/' });
  };

  _updateRouteLocation = ({ route }: { route: string }) => {
    const currentRoute = this.stores.router.location.pathname;
    if (currentRoute !== route) this.stores.router.push(route);
  };

}
