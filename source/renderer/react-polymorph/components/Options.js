// @flow
import React, { Component } from 'react';
import type {
  ComponentType,
  // $FlowFixMe
  SyntheticKeyboardEvent,
  // $FlowFixMe
  SyntheticMouseEvent,
  // $FlowFixMe
  SyntheticEvent,
  Element,
  ElementRef,
} from 'react';

// external libraries
import { isFunction, get, escapeRegExp } from 'lodash';

// internal utility functions
import { createEmptyContext, withTheme } from './HOC/withTheme';
import { composeTheme, addThemeId, didThemePropsChange } from '../utils/themes';
import { composeFunctions } from '../utils/props';

// import constants
import { IDENTIFIERS } from '.';
import type { ThemeContextProp } from './HOC/withTheme';

type Props = {
  className?: String,
  context: ThemeContextProp,
  hasSearch?: boolean,
  hideSearchClearButton?: boolean,
  highlightSearch?: boolean,
  isOpen: boolean,
  isOpeningUpward: boolean,
  noOptionsArrow?: boolean,
  noOptionsCheckmark?: boolean,
  noResults?: boolean,
  noResultsMessage: string | Element<any>,
  onBlur?: Function,
  onChange?: Function,
  onClose?: Function,
  onSearch?: Function,
  optionHeight: ?number,
  options: Array<any>,
  optionRenderer?: Function,
  optionsRef?: ElementRef<any>,
  optionsMaxHeight?: number,
  persistSearchValue?: boolean,
  render?: Function,
  resetOnClose: boolean,
  searchHeight: ?number,
  // TODO: Why do we have two separate props for selection?
  selectedOption?: any,
  selectedOptions?: Array<any>,
  setMouseIsOverOptions?: (boolean) => void,
  skin?: ComponentType<any>,
  targetRef?: ElementRef<*>,
  theme: ?Object, // if passed by user, it will take precedence over this.props.context.theme
  themeId: string,
  themeOverrides: Object,
  toggleMouseLocation?: Function,
  toggleOpen?: Function,
};

type State = {
  composedTheme: Object,
  highlightedOptionIndex: number,
  isMouseOverOptions: boolean,
  searchValue: string,
};

class OptionsBase extends Component<Props, State> {
  // declare ref types
  optionsElement: ?Element<any>; // TODO: Does this get used? Don't think so.

  // define static properties
  static displayName = 'Options';
  static defaultProps = {
    context: createEmptyContext(),
    isOpen: false,
    isOpeningUpward: false,
    noOptionsArrow: false,
    noOptionsCheckmark: false,
    noResultsMessage: 'No results',
    optionHeight: 46,
    options: [],
    resetOnClose: false,
    searchHeight: 52,
    theme: null,
    themeId: IDENTIFIERS.OPTIONS,
    themeOverrides: {},
    toggleOpen() {},
  };

  constructor(props: Props) {
    super(props);

    const { context, themeId, theme, themeOverrides } = props;

    this.searchInputRef = React.createRef();

    this.state = {
      composedTheme: composeTheme(
        addThemeId(theme || context.theme, themeId),
        addThemeId(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      highlightedOptionIndex: 0,
      isMouseOverOptions: false,
    };
  }

  componentDidMount() {
    if (this.props.isOpen) {
      document.addEventListener('keydown', this._handleKeyDown, false);
    }
  }

  componentDidUpdate(prevProps: Props) {
    if (prevProps !== this.props) {
      if (!prevProps.isOpen && this.props.isOpen) {
        this.setupOnOpenListeners();
      } else if (prevProps.isOpen && !this.props.isOpen) {
        this.setupOnCloseListeners();
      }
      didThemePropsChange(prevProps, this.props, this.setState.bind(this));
    }
  }

  componentWillUnmount() {
    document.removeEventListener('keydown', this._handleKeyDown, false);
  }

  setupOnOpenListeners = () => {
    document.addEventListener('keydown', this._handleKeyDown, false);
    const { current: input } = this.searchInputRef;
    if (input) {
      input.focus && input.focus();
      input.select && input.select();
    }
  };

  setupOnCloseListeners = () => {
    document.removeEventListener('keydown', this._handleKeyDown, false);
  };

  close = () => {
    const { isOpen, onClose, resetOnClose, toggleOpen } = this.props;
    if (isOpen && toggleOpen) toggleOpen();
    this.setState({
      highlightedOptionIndex: resetOnClose
        ? 0
        : this.state.highlightedOptionIndex,
    });
    if (onClose) onClose();
  };

  getHighlightedOptionIndex = () => {
    // If nothing is higlighted, highlight selected option
    // In case nothing is selected, highlight first option
    const { options, isOpeningUpward } = this.props;
    const currentIndex = this.state.highlightedOptionIndex;
    let index = 0;

    if (currentIndex !== null) {
      index = currentIndex;
    }

    if (isOpeningUpward) return options.length - 1 - index;
    return index;
  };

  setHighlightedOptionIndex = (optionIndex: number) => {
    if (
      !this.isHighlightedOption(optionIndex) &&
      this.isDisabledOption(optionIndex)
    ) {
      this.setState({ highlightedOptionIndex: optionIndex });
    }
  };

  isSelectedOption = (optionIndex: number) => {
    const { isOpeningUpward } = this.props;
    const options = this.getFilteredOptions() || [];
    const index = isOpeningUpward
      ? options.length - 1 - optionIndex
      : optionIndex;
    const option = options[index];
    return option && this.props.selectedOption === option;
  };

  isHighlightedOption = (optionIndex: number) =>
    this.state.highlightedOptionIndex === optionIndex;

  isDisabledOption = (optionIndex: number) => {
    const { options } = this.props;
    const option = options[optionIndex];
    return option && !option.isDisabled;
  };

  handleClickOnOption = (option: ?Object, event: SyntheticEvent<>) => {
    const { onChange, onBlur, persistSearchValue } = this.props;
    if (option) {
      if (option.isDisabled) return;
      if (onChange) onChange(option, event);
    }
    if (onBlur) onBlur(event);
    if (!persistSearchValue) {
      this.handleClearSearchValue();
    }
    this.close();
  };

  handleSearch = (searchValue: string) => {
    this.setState({
      searchValue,
    });
  };

  handleClearSearchValue = () => {
    this.setState({
      searchValue: '',
    });
  };

  getFilteredOptions = () => {
    const {
      hasSearch,
      onSearch,
      options,
      highlightSearch,
      optionRenderer,
    } = this.props;
    const { searchValue } = this.state;
    if (!hasSearch || !searchValue) {
      return options;
    }
    if (hasSearch && isFunction(onSearch)) {
      return onSearch(searchValue, options);
    }
    const filteredOptions = options.filter((option) => {
      const { label } = option;
      const regex = new RegExp(escapeRegExp(searchValue), 'i');
      return regex.test(label);
    });
    return filteredOptions;
  };

  // returns an object containing props, theme, and method handlers
  // associated with rendering this.props.options, the user can call
  // this in the body of the renderOptions function
  getOptionProps = ({
    onClick,
    onMouseEnter,
    ...rest
  }: { onClick: Function, onMouseEnter: Function } = {}) => {
    const { isOpen, themeId, options, selectedOptions } = this.props;
    const { composedTheme } = this.state;
    const {
      isHighlightedOption,
      isDisabledOption,
      handleClickOnOption,
      setHighlightedOptionIndex,
    } = this;

    return {
      options,
      selectedOptions,
      isOpen,
      isHighlightedOption,
      isDisabledOption,
      theme: composedTheme[themeId],
      onClick: (option: ?Object, event: SyntheticEvent<>) =>
        // the user's custom onClick event handler is composed with
        // the internal functionality of Options (this.handleClickOnOption)
        composeFunctions(onClick, handleClickOnOption)(option, event),
      onMouseEnter: (index: number, event: SyntheticMouseEvent<>) =>
        // user's custom onMouseEnter is composed with this.setHighlightedOptionIndex
        composeFunctions(onMouseEnter, setHighlightedOptionIndex)(index, event),
      ...rest,
    };
  };

  getNoResults = () => {
    const { noResults, hasSearch } = this.props;
    const options = this.getFilteredOptions();
    return noResults || (hasSearch && !options.length);
  };

  // ========= PRIVATE HELPERS =========

  _handleSelectionOnKeyDown = (event: SyntheticKeyboardEvent<>) => {
    const options = this.getFilteredOptions();
    if (options.length) {
      const { isOpeningUpward } = this.props;
      const currentIndex = this.state.highlightedOptionIndex;
      const reverseIndex = options.length - 1 - currentIndex;
      const highlightedOption =
        options[isOpeningUpward ? reverseIndex : currentIndex];
      this.handleClickOnOption(highlightedOption, event);
    } else {
      event.preventDefault();
    }
  };

  _handleHighlightMove = (currentIndex: number, direction: string) => {
    const { options } = this.props;
    if (options.length) {
      const lowerIndexBound = 0;
      const upperIndexBound = options.length - 1;
      let newIndex = direction === 'up' ? currentIndex - 1 : currentIndex + 1;

      // Make sure new index is within options bounds
      newIndex = Math.max(lowerIndexBound, Math.min(newIndex, upperIndexBound));

      if (options[newIndex].isDisabled) {
        // Try to jump over disabled options
        const canMoveUp = newIndex > lowerIndexBound;
        const canMoveDown = newIndex < upperIndexBound;
        if (
          (direction === 'up' && canMoveUp) ||
          (direction === 'down' && canMoveDown)
        ) {
          this._handleHighlightMove(newIndex, direction);
        }
      } else {
        this.setHighlightedOptionIndex(newIndex);
      }
    }
  };

  // this needs to get passed to OptionsSkin and attached to each Option Li
  _handleKeyDown = (event: SyntheticKeyboardEvent<>) => {
    const targetTagName = get(event, 'target.tagName');
    const highlightOptionIndex = this.state.highlightedOptionIndex;
    switch (event.keyCode) {
      case 9: // Tab key: selects currently highlighted option
        event.preventDefault();
        this._handleSelectionOnKeyDown(event);
        break;
      case 13: // Enter key: selects currently highlighted option
        event.preventDefault();
        this._handleSelectionOnKeyDown(event);
        break;
      case 32: // Space key: selects currently highlighted option
        if (targetTagName !== 'INPUT') {
          event.preventDefault();
          this._handleSelectionOnKeyDown(event);
        }
        break;
      case 27: // Escape key: closes options if open
        this.close();
        break;
      case 38: // Up Arrow key: moves highlighted selection 'up' 1 index
        event.preventDefault(); // prevent caret move
        this._handleHighlightMove(highlightOptionIndex, 'up');
        break;
      case 40: // Down Arrow key: moves highlighted selection 'down' 1 index
        event.preventDefault(); // prevent caret move
        this._handleHighlightMove(highlightOptionIndex, 'down');
        break;
      default:
        this.props.resetOnClose && this.setHighlightedOptionIndex(0);
    }
  };

  _setMouseIsOverOptions = (isMouseOverOptions: boolean) => {
    const { toggleMouseLocation, setMouseIsOverOptions } = this.props;
    if (
      this.state.isMouseOverOptions !== isMouseOverOptions &&
      toggleMouseLocation
    ) {
      toggleMouseLocation();
    }
    if (setMouseIsOverOptions) {
      setMouseIsOverOptions(isMouseOverOptions);
    }
    this.setState({
      isMouseOverOptions,
    });
  };

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const {
      highlightSearch,
      skin,
      targetRef,
      theme,
      themeOverrides,
      toggleMouseLocation,
      noResults,
      onChange,
      onSearch,
      options,
      context,
      optionsRef,
      isOpen,
      ...rest
    } = this.props;

    const { composedTheme, highlightedOptionIndex, searchValue } = this.state;

    const OptionsSkin = skin || context.skins[IDENTIFIERS.OPTIONS];

    return (
      <OptionsSkin
        getHighlightedOptionIndex={this.getHighlightedOptionIndex}
        getOptionProps={this.getOptionProps}
        handleClickOnOption={this.handleClickOnOption}
        highlightSearch={highlightSearch}
        highlightedOptionIndex={highlightedOptionIndex}
        isHighlightedOption={this.isHighlightedOption}
        isOpen={isOpen}
        isSelectedOption={this.isSelectedOption}
        options={this.getFilteredOptions()}
        optionsRef={optionsRef}
        onClearSearchValue={this.handleClearSearchValue}
        searchInputRef={this.searchInputRef}
        searchValue={searchValue}
        setHighlightedOptionIndex={this.setHighlightedOptionIndex}
        targetRef={targetRef}
        theme={composedTheme}
        setMouseIsOverOptions={this._setMouseIsOverOptions}
        onSearch={this.handleSearch}
        noResults={this.getNoResults()}
        {...rest}
      />
    );
  }
}

export const Options = withTheme(OptionsBase);
