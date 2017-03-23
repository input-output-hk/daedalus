// @flow
import { observable, action, computed } from 'mobx';
import { defineMessages } from 'react-intl';
import Store from './lib/Store';
import CachedRequest from './lib/CachedRequest';

const messages = defineMessages({
  languageOptionEnglish: {
    id: 'profile.languageSelect.form.options.english',
    defaultMessage: '!!!English',
    description: 'Label for the language select option "English".',
  },
  languageOptionJapanese: {
    id: 'profile.languageSelect.form.options.japanese',
    defaultMessage: '!!!Japanese',
    description: 'Label for the language select option "Japanese".',
  },
  languageOptionChinese: {
    id: 'profile.languageSelect.form.options.chinese',
    defaultMessage: '!!!Chinese',
    description: 'Label for the language select option "Chinese".',
  },
  languageOptionKorean: {
    id: 'profile.languageSelect.form.options.korean',
    defaultMessage: '!!!Korean',
    description: 'Label for the language select option "Korean".',
  },
  languageOptionGerman: {
    id: 'profile.languageSelect.form.options.german',
    defaultMessage: '!!!German',
    description: 'Label for the language select option "German".',
  },
  languageOptionCroatian: {
    id: 'profile.languageSelect.form.options.croatian',
    defaultMessage: '!!!Croatian',
    description: 'Label for the language select option "Croatian".',
  },
});

export default class AppStore extends Store {

  LANGUAGE_OPTIONS = [
    { value: 'en-US', label: messages.languageOptionEnglish },
    { value: 'ja-JP', label: messages.languageOptionJapanese },
    { value: 'zh-CN', label: messages.languageOptionChinese },
    { value: 'ko-KR', label: messages.languageOptionKorean },
    { value: 'de-DE', label: messages.languageOptionGerman },
    { value: 'hr-HR', label: messages.languageOptionCroatian },
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
