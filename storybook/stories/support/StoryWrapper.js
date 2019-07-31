// @flow
import React, { Component, Fragment } from 'react';
import { keys, set } from 'lodash';
import { IntlProvider, addLocaleData } from 'react-intl';
import en from 'react-intl/locale-data/en';
import ja from 'react-intl/locale-data/ja';
import {
  setInitialProps,
  updateParam,
  onReceiveParam,
} from '../../addons/DaedalusMenu';

import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import cardano from '../../../source/renderer/app/themes/daedalus/cardano.js';
import darkBlue from '../../../source/renderer/app/themes/daedalus/dark-blue.js';
import lightBlue from '../../../source/renderer/app/themes/daedalus/light-blue.js';
import darkCardano from '../../../source/renderer/app/themes/daedalus/dark-cardano.js';
import white from '../../../source/renderer/app/themes/daedalus/white.js';
import yellow from '../../../source/renderer/app/themes/daedalus/yellow.js';
import WindowSizeManager from '../../../source/renderer/app/WindowSizeManager';

/* eslint-disable no-restricted-globals */

// // https://github.com/yahoo/react-intl/wiki#loading-locale-data
addLocaleData([...en, ...ja]);

const themes = {
  Cardano: cardano,
  DarkBlue: darkBlue,
  LightBlue: lightBlue,
  DarkCardano: darkCardano,
  Yellow: yellow,
  White: white,
};
const themeNames = keys(themes);

const locales = {
  English: 'en-US',
  Japanese: 'ja-JP',
};
const localeNames = keys(locales);

const operatingSystems = {
  Windows: 'windows',
  Linux: 'linux',
  Mac: 'mac',
};

// These differences are due to the different menu heights on each OS
const osMinWindowHeights = {
  Windows: '541px',
  Linux: '560px',
  Mac: '600px',
};

const osNames = keys(operatingSystems);

type Props = {
  children: any,
};

type State = {
  themeName: string,
  localeName: string,
  osName: string,
};

export default class StoryWrapper extends Component<Props, State> {
  constructor(props: Props) {
    super(props);

    const themeName =
      this.params.get('themeName') ||
      sessionStorage.getItem('themeName') ||
      themeNames[0];
    this.handleSetParam('themeName', themeName);

    const localeName =
      this.params.get('localeName') ||
      sessionStorage.getItem('localeName') ||
      localeNames[0];
    this.handleSetParam('localeName', localeName);

    const osName =
      this.params.get('osName') ||
      sessionStorage.getItem('osName') ||
      osNames[0];
    this.handleSetParam('osName', osName);

    onReceiveParam(this.handleSetParam);

    this.state = {
      themeName,
      localeName,
      osName,
    };
  }

  componentDidMount() {
    const { themeName, localeName, osName } = this.state;
    setInitialProps({
      themeNames,
      localeNames,
      osNames,
      themeName,
      localeName,
      osName,
    });
  }

  get params() {
    const { hash, search } = parent.window.location;
    const queries = hash || search;
    return new URLSearchParams(queries.slice(1));
  }

  setHashParam = (param: string, value: string) => {
    const hash = this.params;
    hash.delete('path');
    hash.set(param, value);
    parent.window.location.hash = hash;
  };

  handleSetParam = (param: string, value: string) => {
    const query = set({}, param, value);
    this.setState(query);
    this.setHashParam(param, value);
    sessionStorage.setItem(param, value);
    updateParam(query);
  };

  render() {
    const { children: Story } = this.props;
    const { themeName, localeName, osName } = this.state;
    const theme = themes[themeName];
    const locale = locales[localeName];
    const minScreenHeight = osMinWindowHeights[osName];

    return (
      <Fragment>
        <ThemeManager variables={theme} />
        <WindowSizeManager minScreenHeight={minScreenHeight} />
        <IntlProvider
          {...{ locale, key: locale, messages: translations[locale] }}
        >
          <Story />
        </IntlProvider>
      </Fragment>
    );
  }
}
