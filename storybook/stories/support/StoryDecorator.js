// @flow
import React, { Component, Children, Fragment } from 'react';
import type { Node } from 'react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { IntlProvider } from 'react-intl';
import { observer } from 'mobx-react';
import { daedalusTheme } from '../../../source/renderer/app/themes/daedalus';
import translations from '../../../source/renderer/app/i18n/translations';
import ThemeManager from '../../../source/renderer/app/ThemeManager';
import theme from '../../../source/renderer/app/themes/daedalus/light-blue.js';
import { themeOverrides } from '../../../source/renderer/app/themes/overrides';

type Props = {
  children: Node,
  propsForChildren?: any,
};

@observer
export default class StoryDecorator extends Component<Props> {
  static defaultProps = {
    propsForChildren: {},
  };

  render() {
    const { children, propsForChildren } = this.props;
    return (
      <Fragment>
        <ThemeManager variables={theme} />
        <IntlProvider
          {...{
            locale: 'en-US',
            key: 'en-US',
            messages: translations['en-US'],
          }}
        >
          <ThemeProvider theme={daedalusTheme} themeOverrides={themeOverrides}>
            <Fragment>
              {Children.map(children, child => {
                const childProps =
                  child.type === 'div' ? {} : { propsForChildren };
                return React.cloneElement(child, childProps);
              })}
            </Fragment>
          </ThemeProvider>
        </IntlProvider>
      </Fragment>
    );
  }
}
