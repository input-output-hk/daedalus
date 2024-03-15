// @ts-nocheck
import React, { Component } from 'react';
import type { ComponentType, Element } from 'react';
// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  context: ThemeContextProp;
  disabled?: boolean;
  label?: string | Element<any>;
  onBlur?: (...args: Array<any>) => any;
  onChange?: (...args: Array<any>) => any;
  onFocus?: (...args: Array<any>) => any;
  selected: boolean;
  skin?: ComponentType<any>;
  theme: Record<string, any> | null | undefined;
  // will take precedence over theme in context if passed
  themeId: string;
  themeOverrides: Record<string, any>;
};
type State = {
  composedTheme: Record<string, any>;
};

class RadioBase extends Component<Props, State> {
  // define static properties
  static displayName = 'Radio';
  static defaultProps = {
    context: createEmptyContext(),
    selected: false,
    theme: null,
    themeId: IDENTIFIERS.RADIO,
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
    const RadioSkin = skin || context.skins[IDENTIFIERS.RADIO];
    return <RadioSkin theme={this.state.composedTheme} {...rest} />;
  }
}

export const Radio = withTheme(RadioBase);
