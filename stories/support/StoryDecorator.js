// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../../app/themes/daedalus';
import translations from '../../app/i18n/translations';
import ThemeManager from '../../app/ThemeManager';

export default class StoryDecorator extends Component {

  props: {
    children: Children,
  };

  render() {
    const { children } = this.props;
    const theme = require(`../../app/themes/daedalus/themeDefault.js`);
    return (
      <div>
        <ThemeManager variables={theme} />
        <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
          <ThemeProvider theme={daedalusTheme}>
            {children}
          </ThemeProvider>
        </IntlProvider>
      </div>
    );
  }

}
