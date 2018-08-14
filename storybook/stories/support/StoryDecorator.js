// @flow
import React, { Component, Children } from 'react';
import type { Node } from 'react';
import { ThemeProvider } from 'react-css-themr';
import { IntlProvider } from 'react-intl';
import { observer } from 'mobx-react';
import { daedalusTheme } from '../../../source/renderer/app/themes/daedalus';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import { THEMES } from '../../../source/renderer/app/themes/index';

type Props = {
  children: Node,
  propsForChildren?: any
};

@observer
export default class StoryDecorator extends Component<Props> {

  render() {
    const { children, propsForChildren } = this.props;
    const theme = require(`../../../source/renderer/app/themes/daedalus/${THEMES.LIGHT_BLUE}.js`);
    return (
      <div>
        <ThemeManager variables={theme} />
        <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
          <ThemeProvider theme={daedalusTheme}>
            <div>
              {Children.map(children, (child) => React.cloneElement(child, propsForChildren))}
            </div>
          </ThemeProvider>
        </IntlProvider>
      </div>
    );
  }

}
