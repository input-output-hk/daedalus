// @flow
import React, { Component } from 'react';
import type { ElementRef, ComponentType, Element } from 'react';

// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';

// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

export type FormFieldProps = {
  className?: ?string,
  context: ThemeContextProp,
  disabled?: boolean,
  error?: string | Element<any>,
  formFieldRef: ElementRef<*>,
  id?: string,
  isErrorHidden?: boolean,
  isErrorShown?: boolean,
  isShowingErrorOnFocus: boolean,
  isShowingErrorOnHover: boolean,
  label?: string | Element<any>,
  onChange: Function,
  render: (setFormFieldRef: (ElementRef<*>) => void) => React$Node,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  themeVariables?: Object,
};

type State = {
  error: string,
  composedTheme: Object,
};

class FormFieldBase extends Component<FormFieldProps, State> {
  // define static properties
  static displayName = 'FormField';

  static defaultProps = {
    context: createEmptyContext(),
    isShowingErrorOnFocus: true,
    isShowingErrorOnHover: true,
    theme: null,
    themeId: IDENTIFIERS.FORM_FIELD,
    themeOverrides: {},
  };

  formFieldRef: ElementRef<*>;

  constructor(props: FormFieldProps) {
    super(props);

    const { context, themeId, theme, themeOverrides } = props;

    this.formFieldRef = props.formFieldRef ?? React.createRef();

    this.state = {
      error: '',
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
    };
  }

  componentDidUpdate(prevProps: FormFieldProps) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  setError = (error: string) => this.setState({ error });

  focusChild = () => {
    const { formFieldRef } = this;
    if (formFieldRef && formFieldRef.current) {
      if (typeof formFieldRef.current.focus === 'function') {
        formFieldRef.current.focus();
      }
    }
  };

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const { skin, theme, themeOverrides, error, context, ...rest } = this.props;

    const FormFieldSkin = skin || context.skins[IDENTIFIERS.FORM_FIELD];

    return (
      <FormFieldSkin
        error={error || this.state.error}
        setError={this.setError}
        theme={this.state.composedTheme}
        formFieldRef={this.formFieldRef}
        focusChild={this.focusChild}
        {...rest}
      />
    );
  }
}

export const FormField = withTheme(FormFieldBase);
