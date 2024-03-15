// @ts-nocheck
import React, { Component } from 'react';
// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  contentLabel: string | Element<any>;
  context: ThemeContextProp;
  isOpen: boolean;
  onClose?: (...args: Array<any>) => any;
  skin?: ComponentType<any>;
  triggerCloseOnOverlayClick: boolean;
  theme: Record<string, any> | null | undefined;
  // will take precedence over theme in context if passed
  themeId: string;
  themeOverrides: Record<string, any>;
};
type State = {
  composedTheme: Record<string, any>;
};

class ModalBase extends Component<Props, State> {
  // define static properties
  static displayName = 'Modal';
  static defaultProps = {
    contentLabel: 'Modal Dialog',
    context: createEmptyContext(),
    isOpen: false,
    triggerCloseOnOverlayClick: true,
    theme: null,
    themeId: IDENTIFIERS.MODAL,
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
    const ModalSkin = skin || context.skins[IDENTIFIERS.MODAL];
    return <ModalSkin theme={this.state.composedTheme} {...rest} />;
  }
}

export const Modal = withTheme(ModalBase);
