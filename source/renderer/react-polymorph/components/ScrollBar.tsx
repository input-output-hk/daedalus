import React, { Component } from 'react';
import type { ComponentType } from 'react';
// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  className?: string;
  context: ThemeContextProp;
  skin?: ComponentType<any>;
  theme: Record<string, any> | null | undefined;
  // will take precedence over theme in context if passed
  themeId: string;
  themeOverrides: Record<string, any>; // custom css/scss from user that adheres to component's theme API
};
type State = {
  composedTheme: Record<string, any>;
};

class ScrollBarBase extends Component<Props, State> {
  // define static properties
  static displayName = 'ScrollBar';
  static defaultProps = {
    context: createEmptyContext(),
    theme: null,
    themeId: IDENTIFIERS.SCROLLBAR,
    themeOverrides: {},
  };

  constructor(props: Props) {
    super(props);
    const { context, themeId, theme, themeOverrides } = props;
    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
    };
  }

  componentDidUpdate(prevProps: Props) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const { skin, context, ...rest } = this.props;
    const ScrollBarSkin = skin || context.skins[IDENTIFIERS.SCROLLBAR];
    return <ScrollBarSkin theme={this.state.composedTheme} {...rest} />;
  }
}

export const ScrollBar = withTheme(ScrollBarBase);
