// @ts-nocheck
import React, { Component } from 'react';
import type { ComponentType } from 'react';
// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  label: string;
  hasIconBefore?: boolean;
  hasIconAfter?: boolean;
  isUnderlined?: boolean;
  underlineOnHover?: boolean;
  onClick?: (...args: Array<any>) => any;
  className?: string;
  context: ThemeContextProp;
  skin?: ComponentType<any>;
  theme: Record<string, any> | null | undefined;
  // will take precedence over theme in context if passed
  themeId: string;
  themeOverrides: Record<string, any>;
};
type State = {
  composedTheme: Record<string, any>;
};

class LinkBase extends Component<Props, State> {
  // define static properties
  static displayName = 'Link';
  static defaultProps = {
    context: createEmptyContext(),
    theme: null,
    themeId: IDENTIFIERS.LINK,
    themeOverrides: {},
    hasIconBefore: false,
    hasIconAfter: true,
    isUnderlined: true,
    underlineOnHover: false,
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
    const LinkSkin = skin || context.skins[this.props.themeId];
    return <LinkSkin theme={this.state.composedTheme} {...rest} />;
  }
}

export const Link = withTheme(LinkBase);
