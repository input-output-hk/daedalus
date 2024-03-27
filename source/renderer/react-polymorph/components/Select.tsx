// @ts-nocheck
import React, { Component } from 'react';
import type { ComponentType, Element } from 'react';
// internal components
import { GlobalListeners } from './HOC/GlobalListeners';
// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  allowBlank: boolean;
  autoFocus: boolean;
  className?: string;
  context: ThemeContextProp;
  disabled?: boolean;
  error?: string | Element<any>;
  hasSearch?: boolean;
  hideSearchClearButton?: boolean;
  highlightSearch?: boolean;
  isOpeningUpward: boolean;
  label?: string | Element<any>;
  noResultsMessage?: string;
  onBlur?: (...args: Array<any>) => any;
  onChange?: (...args: Array<any>) => any;
  onFocus?: (...args: Array<any>) => any;
  onSearch?: (...args: Array<any>) => any;
  optionHeight?: number;
  optionRenderer?: (...args: Array<any>) => any;
  options: Array<any>;
  placeholder?: string;
  selectionRenderer?: (...args: Array<any>) => any;
  searchHeight?: number;
  skin?: ComponentType<any>;
  theme: Record<string, any> | null | undefined;
  // will take precedence over theme in context if passed
  themeId: string;
  themeOverrides: Record<string, any>;
  value: string;
};
type State = {
  composedTheme: Record<string, any>;
  isOpen: boolean;
  mouseIsOverOptions: boolean;
};

class SelectBase extends Component<Props, State> {
  // declare ref types
  rootElement: Element<any> | null | undefined;
  inputElement: Element<'input'>;
  optionsElement: Element<any> | null | undefined;
  // define static properties
  static displayName = 'Select';
  static defaultProps = {
    allowBlank: true,
    autoFocus: false,
    context: createEmptyContext(),
    isOpeningUpward: false,
    options: [],
    theme: null,
    themeOverrides: {},
    themeId: IDENTIFIERS.SELECT,
    value: '',
  };

  constructor(props: Props) {
    super(props);
    // define ref
    this.rootElement = React.createRef();
    this.inputElement = React.createRef();
    this.optionsElement = React.createRef();
    const { context, themeId, theme, themeOverrides } = props;
    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      isOpen: false,
      mouseIsOverOptions: false,
    };
  }

  componentDidMount() {
    // check for autoFocus of input element
    if (this.props.autoFocus) {
      return this.focus();
    }
  }

  componentDidUpdate(prevProps: Props) {
    if (prevProps !== this.props) {
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  // ========= PUBLIC SKIN API =========
  // applying focus to the input element will
  // toggle options open because Select's input is read only
  focus = () => this.toggleOpen();
  toggleOpen = () => {
    if (
      this.state.isOpen &&
      this.optionsElement &&
      this.optionsElement.current
    ) {
      // set Options scroll position to top on close
      this.optionsElement.current.scrollTop = 0;
    }

    this.setState({
      isOpen: !this.state.isOpen,
    });
  };
  toggleMouseLocation = () =>
    this.setState({
      mouseIsOverOptions: !this.state.mouseIsOverOptions,
    });
  handleInputClick = (event: React.MouseEvent) => {
    event.stopPropagation();
    event.preventDefault();
    const { inputElement } = this;

    if (
      inputElement.current &&
      document.activeElement === inputElement.current
    ) {
      inputElement.current.blur();
    }

    this.toggleOpen();
  };
  handleChange = (option: Record<string, any>, event: React.SyntheticEvent) => {
    // check if the user passed an onChange handler and call it
    if (this.props.onChange) this.props.onChange(option.value, event);
    // onChange is called when an option is selected, so close options
    this.toggleOpen();
  };
  getSelectedOption = () => {
    const { options, value, allowBlank } = this.props;

    for (const option of options) {
      if (option.value === value) return option;
    }

    if (!allowBlank) return options[0];
  };

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const {
      skin,
      theme,
      themeOverrides,
      autoFocus,
      context,
      allowBlank,
      optionHeight,
      searchHeight,
      ...rest
    } = this.props;
    const SelectSkin = skin || context.skins[IDENTIFIERS.SELECT];
    return (
      <GlobalListeners
        mouseIsOverOptions={this.state.mouseIsOverOptions}
        optionsIsOpen={this.state.isOpen}
        optionsIsOpeningUpward={this.props.isOpeningUpward}
        optionsRef={this.optionsElement}
        rootRef={this.rootElement}
        toggleOpen={this.toggleOpen}
        hasSearch={this.props.hasSearch}
        optionsLength={this.props.options.length}
      >
        {({ optionsMaxHeight }) => (
          <SelectSkin
            isOpen={this.state.isOpen}
            rootRef={this.rootElement}
            inputRef={this.inputElement}
            optionsRef={this.optionsElement}
            optionsMaxHeight={optionsMaxHeight}
            theme={this.state.composedTheme}
            getSelectedOption={this.getSelectedOption}
            handleInputClick={this.handleInputClick}
            handleChange={this.handleChange}
            toggleOpen={this.toggleOpen}
            toggleMouseLocation={this.toggleMouseLocation}
            optionHeight={optionHeight}
            searchHeight={searchHeight}
            {...rest}
          />
        )}
      </GlobalListeners>
    );
  }
}

export const Select = withTheme(SelectBase);
