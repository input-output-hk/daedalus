// @flow
import React, { Component } from 'react';
import type { Children } from 'react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../../app/themes/daedalus';
import translations from '../../app/i18n/translations';

export default class StoryDecorator extends Component {

  props: {
    children: Children,
  };

  render() {
    const { children } = this.props;
    return (
      <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
        <ThemeProvider theme={daedalusTheme}>
          {children}
        </ThemeProvider>
      </IntlProvider>
    );
  }

}
