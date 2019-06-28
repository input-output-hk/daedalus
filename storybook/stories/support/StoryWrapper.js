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

/* eslint-disable no-restricted-globals */

// // https://github.com/yahoo/react-intl/wiki#loading-locale-data
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
};

export default class StoryWrapper extends Component<Props, State> {
  constructor(props: Props) {
    super(props);

    /**
     *
     * themeNameSession and localeNameSession:
     *
     * the sessionStorage variables are necessary
     * because it's impossible to change the Storybook sidebar links behaviour
     * so when click a sidebar link, the sessionStorage variable will take advantage
     *
     */
    const themeNameSession = sessionStorage.getItem('themeName');
    const localeNameSession = sessionStorage.getItem('localeName');

    const themeName =
      themeNameSession || this.params.get('themeName') || themeNames[0];
    const localeName =
      localeNameSession || this.params.get('localeName') || localeNames[0];

    // this.updateQueryParam({ themeName, localeName });

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
    return new URLSearchParams(parent.window.location.search);
  }

  handleSetParam = (param: string, value: string) => {
    const query = set({}, param, value);
    this.setState(query);
    sessionStorage.setItem(param, value);
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
