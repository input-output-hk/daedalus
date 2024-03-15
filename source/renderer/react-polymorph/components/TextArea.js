// @flow
import React, { Component } from 'react';
import type { ComponentType, Node, Element } from 'react';

// external libraries
import { isString, flow } from 'lodash';

// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';

// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  autoFocus: boolean,
  autoResize: boolean,
  className?: string,
  context: ThemeContextProp,
  disabled?: boolean,
  label?: string | Element<any>,
  error?: string | Node,
  maxLength?: number,
  minLength?: number,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  placeholder?: string,
  rows?: number,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  value: string,
};

type State = {
  error: string,
  composedTheme: Object,
};

class TextAreaBase extends Component<Props, State> {
  // declare ref types
  textareaElement: Element<'textarea'>;

  // define static properties
  static displayName = 'TextArea';
  static defaultProps = {
    autoFocus: false,
    autoResize: true,
    context: createEmptyContext(),
    theme: null,
    themeId: IDENTIFIERS.TEXT_AREA,
    themeOverrides: {},
    value: '',
  };

  constructor(props: Props) {
    super(props);

    // define ref
    this.textareaElement = React.createRef();

    const { context, themeId, theme, themeOverrides } = props;

    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      error: '',
    };
  }

  componentDidMount() {
    const { autoResize, autoFocus } = this.props;

    if (autoResize) {
      window.addEventListener('resize', this._handleAutoresize);
      this._handleAutoresize();
    }

    if (autoFocus) {
      this.focus();
    }
  }

  componentDidUpdate(prevProps: Props) {
    if (this.props.autoResize) this._handleAutoresize();

    if (prevProps !== this.props) {
      if (!prevProps.autoResize && this.props.autoResize) {
        window.addEventListener('resize', this._handleAutoresize);
      } else if (prevProps.autoResize && !this.props.autoResize) {
        window.removeEventListener('resize', this._handleAutoresize);
      }

      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  componentWillUnmount() {
    if (this.props.autoResize) {
      window.removeEventListener('resize', this._handleAutoresize);
    }
  }

  focus = () => {
    const { textareaElement } = this;
    if (!textareaElement.current) return;
    textareaElement.current.focus();
  };

  onChange = (event: SyntheticInputEvent<>) => {
    const { onChange, disabled } = this.props;
    if (disabled) return;

    if (onChange) onChange(this._processValue(event.target.value), event);
  };

  _setError = (error: string) => this.setState({ error });

  _processValue(value: string) {
    return flow([
      this._enforceStringValue,
      this._enforceMaxLength,
      this._enforceMinLength,
    ]).call(this, value);
  }

  _enforceStringValue(value: string) {
    if (!isString(value)) {
      throw new Error('Values passed to TextArea::onChange must be strings');
    }
    return value;
  }

  _enforceMaxLength(value: string) {
    const { maxLength } = this.props;
    const isTooLong = maxLength != null && value.length > maxLength;
    return isTooLong ? value.substring(0, maxLength) : value;
  }

  _enforceMinLength(value: string) {
    const { minLength } = this.props;
    const isTooShort = minLength != null && value.length < minLength;

    if (isTooShort) {
      this._setError('Please enter a valid input');
    } else if (this.state.error !== '') {
      this._setError('');
    }

    return value;
  }

  _handleAutoresize() {
    const { textareaElement } = this;

    if (!textareaElement.current) return;

    // compute the height difference between inner height and outer height
    const style = getComputedStyle(textareaElement.current, '');
    const heightOffset =
      style.boxSizing === 'content-box'
        ? -(parseFloat(style.paddingTop) + parseFloat(style.paddingBottom))
        : parseFloat(style.borderTopWidth) +
          parseFloat(style.borderBottomWidth);

    // resize the input to its content size
    textareaElement.current.style.height = 'auto';
    textareaElement.current.style.height = `${
      textareaElement.current.scrollHeight + heightOffset
    }px`;
  }

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const {
      skin,
      theme,
      themeOverrides,
      onChange,
      error,
      context,
      autoFocus,
      autoResize,
      ...rest
    } = this.props;

    const TextAreaSkin = skin || context.skins[IDENTIFIERS.TEXT_AREA];

    return (
      <TextAreaSkin
        error={error || this.state.error}
        onChange={this.onChange}
        textareaRef={this.textareaElement}
        theme={this.state.composedTheme}
        {...rest}
      />
    );
  }
}

export const TextArea = withTheme(TextAreaBase);
