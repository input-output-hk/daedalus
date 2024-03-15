'use strict';
var __extends =
  (this && this.__extends) ||
  (function () {
    var extendStatics = function (d, b) {
      extendStatics =
        Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array &&
          function (d, b) {
            d.__proto__ = b;
          }) ||
        function (d, b) {
          for (var p in b)
            if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p];
        };
      return extendStatics(d, b);
    };
    return function (d, b) {
      if (typeof b !== 'function' && b !== null)
        throw new TypeError(
          'Class extends value ' + String(b) + ' is not a constructor or null'
        );
      extendStatics(d, b);
      function __() {
        this.constructor = d;
      }
      d.prototype =
        b === null
          ? Object.create(b)
          : ((__.prototype = b.prototype), new __());
    };
  })();
var __assign =
  (this && this.__assign) ||
  function () {
    __assign =
      Object.assign ||
      function (t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
          s = arguments[i];
          for (var p in s)
            if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
        }
        return t;
      };
    return __assign.apply(this, arguments);
  };
var __rest =
  (this && this.__rest) ||
  function (s, e) {
    var t = {};
    for (var p in s)
      if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === 'function')
      for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
        if (
          e.indexOf(p[i]) < 0 &&
          Object.prototype.propertyIsEnumerable.call(s, p[i])
        )
          t[p[i]] = s[p[i]];
      }
    return t;
  };
exports.__esModule = true;
exports.Options = void 0;
// @ts-nocheck
var react_1 = require('react');
// external libraries
var lodash_1 = require('lodash');
// internal utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
var props_1 = require('../utils/props');
// import constants
var _1 = require('.');
var OptionsBase = /** @class */ (function (_super) {
  __extends(OptionsBase, _super);
  function OptionsBase(props) {
    var _this = _super.call(this, props) || this;
    _this.setupOnOpenListeners = function () {
      document.addEventListener('keydown', _this._handleKeyDown, false);
      var input = _this.searchInputRef.current;
      if (input) {
        input.focus && input.focus();
        input.select && input.select();
      }
    };
    _this.setupOnCloseListeners = function () {
      document.removeEventListener('keydown', _this._handleKeyDown, false);
    };
    _this.close = function () {
      var _a = _this.props,
        isOpen = _a.isOpen,
        onClose = _a.onClose,
        resetOnClose = _a.resetOnClose,
        toggleOpen = _a.toggleOpen;
      if (isOpen && toggleOpen) toggleOpen();
      _this.setState({
        highlightedOptionIndex: resetOnClose
          ? 0
          : _this.state.highlightedOptionIndex,
      });
      if (onClose) onClose();
    };
    _this.getHighlightedOptionIndex = function () {
      // If nothing is higlighted, highlight selected option
      // In case nothing is selected, highlight first option
      var _a = _this.props,
        options = _a.options,
        isOpeningUpward = _a.isOpeningUpward;
      var currentIndex = _this.state.highlightedOptionIndex;
      var index = 0;
      if (currentIndex !== null) {
        index = currentIndex;
      }
      if (isOpeningUpward) return options.length - 1 - index;
      return index;
    };
    _this.setHighlightedOptionIndex = function (optionIndex) {
      if (
        !_this.isHighlightedOption(optionIndex) &&
        _this.isDisabledOption(optionIndex)
      ) {
        _this.setState({
          highlightedOptionIndex: optionIndex,
        });
      }
    };
    _this.isSelectedOption = function (optionIndex) {
      var isOpeningUpward = _this.props.isOpeningUpward;
      var options = _this.getFilteredOptions() || [];
      var index = isOpeningUpward
        ? options.length - 1 - optionIndex
        : optionIndex;
      var option = options[index];
      return option && _this.props.selectedOption === option;
    };
    _this.isHighlightedOption = function (optionIndex) {
      return _this.state.highlightedOptionIndex === optionIndex;
    };
    _this.isDisabledOption = function (optionIndex) {
      var options = _this.props.options;
      var option = options[optionIndex];
      return option && !option.isDisabled;
    };
    _this.handleClickOnOption = function (option, event) {
      var _a = _this.props,
        onChange = _a.onChange,
        onBlur = _a.onBlur,
        persistSearchValue = _a.persistSearchValue;
      if (option) {
        if (option.isDisabled) return;
        if (onChange) onChange(option, event);
      }
      if (onBlur) onBlur(event);
      if (!persistSearchValue) {
        _this.handleClearSearchValue();
      }
      _this.close();
    };
    _this.handleSearch = function (searchValue) {
      _this.setState({
        searchValue: searchValue,
      });
    };
    _this.handleClearSearchValue = function () {
      _this.setState({
        searchValue: '',
      });
    };
    _this.getFilteredOptions = function () {
      var _a = _this.props,
        hasSearch = _a.hasSearch,
        onSearch = _a.onSearch,
        options = _a.options;
      var searchValue = _this.state.searchValue;
      if (!hasSearch || !searchValue) {
        return options;
      }
      if (hasSearch && (0, lodash_1.isFunction)(onSearch)) {
        return onSearch(searchValue, options);
      }
      var filteredOptions = options.filter(function (option) {
        var label = option.label;
        var regex = new RegExp((0, lodash_1.escapeRegExp)(searchValue), 'i');
        return regex.test(label);
      });
      return filteredOptions;
    };
    // returns an object containing props, theme, and method handlers
    // associated with rendering this.props.options, the user can call
    // this in the body of the renderOptions function
    _this.getOptionProps = function (_a) {
      if (_a === void 0) {
        _a = {};
      }
      var onClick = _a.onClick,
        onMouseEnter = _a.onMouseEnter,
        rest = __rest(_a, ['onClick', 'onMouseEnter']);
      var _b = _this.props,
        isOpen = _b.isOpen,
        themeId = _b.themeId,
        options = _b.options,
        selectedOptions = _b.selectedOptions;
      var composedTheme = _this.state.composedTheme;
      var _c = _this,
        isHighlightedOption = _c.isHighlightedOption,
        isDisabledOption = _c.isDisabledOption,
        handleClickOnOption = _c.handleClickOnOption,
        setHighlightedOptionIndex = _c.setHighlightedOptionIndex;
      return __assign(
        {
          options: options,
          selectedOptions: selectedOptions,
          isOpen: isOpen,
          isHighlightedOption: isHighlightedOption,
          isDisabledOption: isDisabledOption,
          theme: composedTheme[themeId],
          onClick: function (
            option,
            event // the user's custom onClick event handler is composed with
          ) {
            // the internal functionality of Options (this.handleClickOnOption)
            return (0, props_1.composeFunctions)(onClick, handleClickOnOption)(
              option,
              event
            );
          },
          onMouseEnter: function (
            index,
            event // user's custom onMouseEnter is composed with this.setHighlightedOptionIndex
          ) {
            return (0, props_1.composeFunctions)(
              onMouseEnter,
              setHighlightedOptionIndex
            )(index, event);
          },
        },
        rest
      );
    };
    _this.getNoResults = function () {
      var _a = _this.props,
        noResults = _a.noResults,
        hasSearch = _a.hasSearch;
      var options = _this.getFilteredOptions();
      return noResults || (hasSearch && !options.length);
    };
    // ========= PRIVATE HELPERS =========
    _this._handleSelectionOnKeyDown = function (event) {
      var options = _this.getFilteredOptions();
      if (options.length) {
        var isOpeningUpward = _this.props.isOpeningUpward;
        var currentIndex = _this.state.highlightedOptionIndex;
        var reverseIndex = options.length - 1 - currentIndex;
        var highlightedOption =
          options[isOpeningUpward ? reverseIndex : currentIndex];
        _this.handleClickOnOption(highlightedOption, event);
      } else {
        event.preventDefault();
      }
    };
    _this._handleHighlightMove = function (currentIndex, direction) {
      var options = _this.props.options;
      if (options.length) {
        var lowerIndexBound = 0;
        var upperIndexBound = options.length - 1;
        var newIndex = direction === 'up' ? currentIndex - 1 : currentIndex + 1;
        // Make sure new index is within options bounds
        newIndex = Math.max(
          lowerIndexBound,
          Math.min(newIndex, upperIndexBound)
        );
        if (options[newIndex].isDisabled) {
          // Try to jump over disabled options
          var canMoveUp = newIndex > lowerIndexBound;
          var canMoveDown = newIndex < upperIndexBound;
          if (
            (direction === 'up' && canMoveUp) ||
            (direction === 'down' && canMoveDown)
          ) {
            _this._handleHighlightMove(newIndex, direction);
          }
        } else {
          _this.setHighlightedOptionIndex(newIndex);
        }
      }
    };
    // this needs to get passed to OptionsSkin and attached to each Option Li
    _this._handleKeyDown = function (event) {
      var targetTagName = (0, lodash_1.get)(event, 'target.tagName');
      var highlightOptionIndex = _this.state.highlightedOptionIndex;
      switch (event.keyCode) {
        case 9:
          // Tab key: selects currently highlighted option
          event.preventDefault();
          _this._handleSelectionOnKeyDown(event);
          break;
        case 13:
          // Enter key: selects currently highlighted option
          event.preventDefault();
          _this._handleSelectionOnKeyDown(event);
          break;
        case 32:
          // Space key: selects currently highlighted option
          if (targetTagName !== 'INPUT') {
            event.preventDefault();
            _this._handleSelectionOnKeyDown(event);
          }
          break;
        case 27:
          // Escape key: closes options if open
          _this.close();
          break;
        case 38:
          // Up Arrow key: moves highlighted selection 'up' 1 index
          event.preventDefault(); // prevent caret move
          _this._handleHighlightMove(highlightOptionIndex, 'up');
          break;
        case 40:
          // Down Arrow key: moves highlighted selection 'down' 1 index
          event.preventDefault(); // prevent caret move
          _this._handleHighlightMove(highlightOptionIndex, 'down');
          break;
        default:
          _this.props.resetOnClose && _this.setHighlightedOptionIndex(0);
      }
    };
    _this._setMouseIsOverOptions = function (isMouseOverOptions) {
      var _a = _this.props,
        toggleMouseLocation = _a.toggleMouseLocation,
        setMouseIsOverOptions = _a.setMouseIsOverOptions;
      if (
        _this.state.isMouseOverOptions !== isMouseOverOptions &&
        toggleMouseLocation
      ) {
        toggleMouseLocation();
      }
      if (setMouseIsOverOptions) {
        setMouseIsOverOptions(isMouseOverOptions);
      }
      _this.setState({
        isMouseOverOptions: isMouseOverOptions,
      });
    };
    var context = props.context,
      themeId = props.themeId,
      theme = props.theme,
      themeOverrides = props.themeOverrides;
    _this.searchInputRef = react_1['default'].createRef();
    _this.state = {
      composedTheme: (0, themes_1.composeTheme)(
        (0, themes_1.addThemeId)(theme || context.theme, themeId),
        (0, themes_1.addThemeId)(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      highlightedOptionIndex: 0,
      isMouseOverOptions: false,
    };
    return _this;
  }
  OptionsBase.prototype.componentDidMount = function () {
    if (this.props.isOpen) {
      document.addEventListener('keydown', this._handleKeyDown, false);
    }
  };
  OptionsBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      if (!prevProps.isOpen && this.props.isOpen) {
        this.setupOnOpenListeners();
      } else if (prevProps.isOpen && !this.props.isOpen) {
        this.setupOnCloseListeners();
      }
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  OptionsBase.prototype.componentWillUnmount = function () {
    document.removeEventListener('keydown', this._handleKeyDown, false);
  };
  OptionsBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    var _a = this.props,
      highlightSearch = _a.highlightSearch,
      skin = _a.skin,
      targetRef = _a.targetRef,
      context = _a.context,
      optionsRef = _a.optionsRef,
      isOpen = _a.isOpen,
      rest = __rest(_a, [
        'highlightSearch',
        'skin',
        'targetRef',
        'context',
        'optionsRef',
        'isOpen',
      ]);
    var _b = this.state,
      composedTheme = _b.composedTheme,
      highlightedOptionIndex = _b.highlightedOptionIndex,
      searchValue = _b.searchValue;
    var OptionsSkin = skin || context.skins[_1.IDENTIFIERS.OPTIONS];
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
  };
  // define static properties
  OptionsBase.displayName = 'Options';
  OptionsBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
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
    themeId: _1.IDENTIFIERS.OPTIONS,
    themeOverrides: {},
    toggleOpen: function () {},
  };
  return OptionsBase;
})(react_1.Component);
exports.Options = (0, withTheme_1.withTheme)(OptionsBase);
