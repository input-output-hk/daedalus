// @flow
import React, { Component } from 'react';
import type { ComponentType } from 'react';

// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';

// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  className?: string,
  context: ThemeContextProp,
  label?: string,
  progress: number,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object, // custom css/scss from user that adheres to component's theme API
};

type State = {
  composedTheme: Object,
};

class ProgressBarBase extends Component<Props, State> {
  // define static properties
  static displayName = 'ProgressBar';
  static defaultProps = {
    context: createEmptyContext(),
    progress: 100,
    theme: null,
    themeId: IDENTIFIERS.PROGRESS_BAR,
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
    const { skin, theme, themeOverrides, context, ...rest } = this.props;

    const ProgressBarSkin = skin || context.skins[IDENTIFIERS.PROGRESS_BAR];

    return <ProgressBarSkin theme={this.state.composedTheme} {...rest} />;
  }
}

export const ProgressBar = withTheme(ProgressBarBase);
