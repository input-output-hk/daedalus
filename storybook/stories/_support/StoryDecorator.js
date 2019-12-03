// @flow
import React, { Component, Children, Fragment } from 'react';
import type { Node } from 'react';
import { observer } from 'mobx-react';
import { ThemeProvider } from 'react-polymorph/lib/components/ThemeProvider';
import { SimpleSkins } from 'react-polymorph/lib/skins/simple';
import { daedalusTheme } from '../../../source/renderer/app/themes/daedalus';
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
      <ThemeProvider
        theme={daedalusTheme}
        skins={SimpleSkins}
        themeOverrides={themeOverrides}
      >
        <Fragment>
          {Children.map(children, child => {
            const childProps = child.type === 'div' ? {} : { propsForChildren };
            return React.cloneElement(child, childProps);
          })}
        </Fragment>
      </ThemeProvider>
    );
  }
}
