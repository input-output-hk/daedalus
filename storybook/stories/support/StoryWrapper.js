// @flow
import React, { Component, Fragment } from 'react';
import { keys, set } from 'lodash';
import { IntlProvider, addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';

import DaedalusMenu from './DaedalusMenu';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import cardano from '../../../source/renderer/app/themes/daedalus/cardano.js';
import darkBlue from '../../../source/renderer/app/themes/daedalus/dark-blue.js';
import lightBlue from '../../../source/renderer/app/themes/daedalus/light-blue.js';

/* eslint-disable no-restricted-globals */

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
  constructor(props) {
    super(props);

    const themeName = this.params.get('themeName') || themeNames[0];
    const localeName = this.params.get('localeName') || localeNames[0];

    this.state = {
      themeName,
      localeName,
      isMenuVisible: false,
    };
  }

  get params() {
    return new URLSearchParams(parent.window.location.search);
  }

  setParam = (param: string, value: string) => {
    this.setState(set({}, param, value));
    const { params } = this;
    params.set(param, value);
    const { location } = parent.window;
    const newurl = `${location.protocol}//${location.host}${
      location.pathname
    }?${params.toString()}`;
    parent.window.history.pushState({ path: newurl }, '', newurl);
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
          setParam={this.setParam}
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
