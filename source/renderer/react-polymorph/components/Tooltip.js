// @flow
import React, { Component } from 'react';
import type { ComponentType, Element, Node } from 'react';

// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';

// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

export type TooltipProps = {
  children: ?Node,
  className?: string,
  context: ThemeContextProp,
  isAligningRight?: boolean,
  isBounded: boolean,
  isCentered: boolean,
  isOpeningUpward: boolean,
  isShowingOnHover: boolean,
  isTransparent: boolean,
  isVisible: boolean,
  arrowRelativeToTip: boolean,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeOverrides: Object, // custom css/scss from user that adheres to component's theme API
  themeId: string,
  tip?: string | Element<any>,
};

type State = {
  composedTheme: Object,
};

class TooltipBase extends Component<TooltipProps, State> {
  // define static properties
  static displayName = 'Tooltip';
  static defaultProps = {
    context: createEmptyContext(),
    isBounded: false,
    isCentered: false,
    isOpeningUpward: true,
    isShowingOnHover: true,
    isTransparent: true,
    isVisible: false,
    arrowRelativeToTip: false,
    theme: null,
    themeId: IDENTIFIERS.TOOLTIP,
    themeOverrides: {},
  };

  constructor(props: TooltipProps) {
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

  componentDidUpdate(prevProps: TooltipProps) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const { skin, theme, themeOverrides, context, ...rest } = this.props;

    const TooltipSkin = skin || context.skins[IDENTIFIERS.TOOLTIP];

    return <TooltipSkin theme={this.state.composedTheme} {...rest} />;
  }
}

export const Tooltip = withTheme(TooltipBase);
