// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../../app/themes/daedalus';
import translations from '../../app/i18n/translations';
import ThemeManager from '../../app/ThemeManager';
import { THEMES } from '../../app/themes/index';

export default class StoryDecorator extends Component {

  props: {
    children: Children,
  };

  render() {
    const { children } = this.props;
    const theme = require(`../../app/themes/daedalus/${THEMES.LIGHT_BLUE}.js`);
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
