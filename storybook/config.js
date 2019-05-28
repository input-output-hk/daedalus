// @flow
import React, { Fragment } from 'react';
import { configure, addDecorator } from '@storybook/react';
import { select, withKnobs } from '@storybook/addon-knobs';
import { keys } from 'lodash';
import { IntlProvider, addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';
import translations from '../source/renderer/app/i18n/translations';
import ThemeManager from '../source/renderer/app/ThemeManager';
import cardano from '../source/renderer/app/themes/daedalus/cardano.js';
import darkBlue from '../source/renderer/app/themes/daedalus/dark-blue.js';
import lightBlue from '../source/renderer/app/themes/daedalus/light-blue.js';

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

// Clear current theme and locale selection between sessions
localStorage.removeItem('currentTheme');
localStorage.removeItem('currentLocale');

addDecorator((story, context) => {
  const storyWithKnobs = withKnobs(story, context);

  // Load current theme selection
  const currentTheme = localStorage.getItem('currentTheme') || themeNames[0];
  const themeName = select('Theme', themeNames, currentTheme);
  const theme = themes[themeName];
  localStorage.setItem('currentTheme', themeName);

  // Load current locale selection
  const currentLocale = localStorage.getItem('currentLocale') || localeNames[0];
  const localeName = select('Language', localeNames, currentLocale);
  const locale = locales[localeName];
  localStorage.setItem('currentLocale', localeName);

  return (
    <Fragment>
      <ThemeManager variables={theme} />
      <IntlProvider
        {...{ locale, key: locale, messages: translations[locale] }}
      >
        {storyWithKnobs}
      </IntlProvider>
    </Fragment>
  );
});

function loadStories() {
  require('./stories');
}

configure(loadStories, module);
