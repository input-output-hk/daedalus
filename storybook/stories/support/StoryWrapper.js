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

type Props = {
  children: any,
};

type State = {
  themeName: string,
  localeName: string,
};

export default class StoryWrapper extends Component<Props, State> {
  constructor(props: Props) {
    super(props);

    const themeName =
      this.params.get('themeName') ||
      localStorage.getItem('themeName') ||
      themeNames[0];
    const localeName =
      this.params.get('localeName') ||
      localStorage.getItem('localeName') ||
      localeNames[0];

    onReceiveParam(this.handleSetParam);

    this.state = {
      themeName,
      localeName,
    };
  }

  componentDidMount() {
    const { themeName, localeName } = this.state;
    setInitialProps({
      themes,
      themeNames,
      locales,
      localeNames,
      themeName,
      localeName,
    });
  }

  get params() {
    return new URLSearchParams(parent.window.location.search.slice(1));
  }

  handleSetParam = (param: string, value: string) => {
    const query = set({}, param, value);
    this.setState(query);
    localStorage.setItem(param, value);
    updateParam(query);
  };

  render() {
    const { children: Story } = this.props;
    const { themeName, localeName } = this.state;
    const theme = themes[themeName];
    const locale = locales[localeName];

    return (
      <Fragment>
        <ThemeManager variables={theme} />
        <IntlProvider
          {...{ locale, key: locale, messages: translations[locale] }}
        >
          <Story />
        </IntlProvider>
      </Fragment>
    );
  }
}
