// @flow
import React, { Component, PropTypes } from 'react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../../app/themes/daedalus';
import translations from '../../app/i18n/translations';

export default class StoryDecorator extends Component {

  static propTypes = {
    children: PropTypes.oneOfType([
      PropTypes.arrayOf(PropTypes.element),
      PropTypes.element
    ]).isRequired
  };

  render() {
    const { children } = this.props;
    return (
      <div>
        <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
          <ThemeProvider theme={daedalusTheme}>
            {children}
          </ThemeProvider>
        </IntlProvider>
      </div>
    );
  }

}
