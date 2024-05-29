// @ts-nocheck
import React from 'react';
import type { ComponentType, Ref } from 'react';
import { ThemeContext } from './ThemeContext';
import { getDisplayName } from '../../utils/props';

export type ThemeContextProp = {
  skins: Record<string, any>;
  theme: Record<string, any>;
  ROOT_THEME_API: Record<string, any>;
};
export function createEmptyContext(): ThemeContextProp {
  return {
    skins: {},
    theme: {},
    ROOT_THEME_API: {},
  };
}
// withTheme is a HOC that takes a Component as a parameter
// and returns that Component wrapped within ThemeContext.Consumer.
// Any additional props and refs are forwarded to the returned Component.
export function withTheme<C extends ComponentType<any>>(Component: C): C {
  let WrappedComponent;

  if (process.env.NODE_ENV === 'test') {
    // wraps component in context only
    WrappedComponent = function (props: {}) {
      return (
        <ThemeContext.Consumer>
          {(context) => <Component context={context} {...props} />}
        </ThemeContext.Consumer>
      );
    };
  } else {
    // wraps component in context AND forwardRef
    WrappedComponent = React.forwardRef((props: {}, ref: Ref<any>) => (
      <ThemeContext.Consumer>
        {(context) => <Component context={context} ref={ref} {...props} />}
      </ThemeContext.Consumer>
    ));
  }

  // create a new displayName for the wrapped component
  WrappedComponent.displayName = `withTheme(${getDisplayName(Component)})`;
  // Cast type to our desired custom component
  return (WrappedComponent as any) as C;
}
