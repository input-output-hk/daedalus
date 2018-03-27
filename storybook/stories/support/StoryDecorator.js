// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../../../source/renderer/app/themes/daedalus';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import { THEMES } from '../../../source/renderer/app/themes/index';

type Props = {
  children: Node,
};

export default class StoryDecorator extends Component<Props> {

  render() {
    const { children } = this.props;
    const theme = require(`../../../source/renderer/app/themes/daedalus/${THEMES.LIGHT_BLUE}.js`);
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
