// @flow
import React, { Component, Fragment } from 'react';
import { keys } from 'lodash';
import { IntlProvider, addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';
import DaedalusMenu from './DaedalusMenu';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import cardano from '../../../source/renderer/app/themes/daedalus/cardano.js';
import darkBlue from '../../../source/renderer/app/themes/daedalus/dark-blue.js';
import lightBlue from '../../../source/renderer/app/themes/daedalus/light-blue.js';

// https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([...en, ...ja]);

const themes = {
  Cardano: cardano,
  DarkBlue: darkBlue,
  LightBlue: lightBlue,
};
const themeNames = keys(themes);

const locales = {
  English: 'en-US',
  Japanese: 'ja-JP',
};
const localeNames = keys(locales);

type Props = {
  children: any,
};

type State = {
  themeName: string,
  localeName: string,
  isMenuVisible: boolean,
};

export default class StoryWrapper extends Component<Props, State> {
  state = {
    themeName: localStorage.getItem('currentTheme') || themeNames[0],
    localeName: localStorage.getItem('currentLocale') || localeNames[0],
    isMenuVisible: false,
  };

  setLocaleName = (localeName: string) => {
    this.setState({ localeName });
    localStorage.setItem('currentLocale', localeName);
  };

  setThemeName = (themeName: string) => {
    this.setState({ themeName });
    localStorage.setItem('currentTheme', themeName);
  };

  handleToggleVisibility = () =>
    this.setState(({ isMenuVisible }) => ({ isMenuVisible: !isMenuVisible }));

  render() {
    const { children: Story } = this.props;
    const { themeName, localeName, isMenuVisible } = this.state;
    const theme = themes[themeName];
    const locale = locales[localeName];

    return (
      <Fragment>
        <ThemeManager variables={theme} />
        <DaedalusMenu
          localeNames={localeNames}
          themeNames={themeNames}
          setLocaleName={this.setLocaleName}
          setThemeName={this.setThemeName}
          currentLocale={localeName}
          currentTheme={themeName}
          onToggleVisibility={this.handleToggleVisibility}
          isVisible={isMenuVisible}
        />

        <IntlProvider
          {...{ locale, key: locale, messages: translations[locale] }}
        >
          <Story />
        </IntlProvider>
      </Fragment>
    );
  }
}
