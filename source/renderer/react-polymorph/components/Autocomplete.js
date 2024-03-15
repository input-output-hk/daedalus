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
var __spreadArray =
  (this && this.__spreadArray) ||
  function (to, from, pack) {
    if (pack || arguments.length === 2)
      for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
          if (!ar) ar = Array.prototype.slice.call(from, 0, i);
          ar[i] = from[i];
        }
      }
    return to.concat(ar || Array.prototype.slice.call(from));
  };
exports.__esModule = true;
exports.Autocomplete = void 0;
// @ts-nocheck
var react_1 = require('react');
// external libraries
var lodash_1 = require('lodash');
// interal components
var GlobalListeners_1 = require('./HOC/GlobalListeners');
// internal utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
var props_1 = require('../utils/props');
var _1 = require('.');
var AutocompleteBase = /** @class */ (function (_super) {
  __extends(AutocompleteBase, _super);
  function AutocompleteBase(props) {
    var _this = _super.call(this, props) || this;
    _this.clear = function () {
      return _this._removeOptions();
    };
    _this.focus = function () {
      return _this.handleAutocompleteClick();
    };
    _this.open = function () {
      return _this.setState({
        isOpen: true,
      });
    };
    _this.close = function () {
      return _this.setState({
        isOpen: false,
      });
    };
    _this.toggleOpen = function () {
      if (
        _this.state.isOpen &&
        _this.optionsElement &&
        _this.optionsElement.current
      ) {
        // set Options scroll position to top on close
        _this.optionsElement.current.scrollTop = 0;
      }
      _this.setState(function (prevState) {
        return {
          isOpen: !prevState.isOpen,
        };
      });
    };
    _this.toggleMouseLocation = function () {
      return _this.setState(function (prevState) {
        return {
          mouseIsOverOptions: !prevState.mouseIsOverOptions,
        };
      });
    };
    _this.handleAutocompleteClick = function () {
      var inputElement = _this.inputElement;
      if (inputElement && inputElement.current) {
        inputElement.current.focus();
      }
      // toggle options open/closed
      _this.toggleOpen();
    };
    _this.onKeyDown = function (event) {
      if (
        // Check for backspace in order to delete the last selected option
        event.keyCode === 8 &&
        !event.target.value &&
        _this.state.selectedOptions.length
      ) {
        // Remove last selected option
        _this.removeOption(_this.state.selectedOptions.length - 1, event);
      } else if (event.keyCode === 27) {
        // ESCAPE key: Stops propagation & modal closing
        event.stopPropagation();
      } else if (event.keyCode === 13) {
        // ENTER key: Opens suggestions
        _this.open();
      }
    };
    // onChange handler for input element in AutocompleteSkin
    _this.handleInputChange = function (event) {
      var value = event.target.value;
      var multipleValues = value.split(' ');
      var hasMultipleValues = multipleValues.length > 1;
      _this._setInputValue(value);
      if (hasMultipleValues) {
        _this.open();
        setTimeout(function () {
          _this.updateSelectedOptions(event, multipleValues);
        }, 0);
      }
    };
    // passed to Options onChange handler in AutocompleteSkin
    _this.handleChange = function (option, event) {
      _this.updateSelectedOptions(event, option);
    };
    _this.updateSelectedOptions = function (event, selectedOption) {
      if (selectedOption === void 0) {
        selectedOption = null;
      }
      var _a = _this.props,
        maxSelections = _a.maxSelections,
        multipleSameSelections = _a.multipleSameSelections,
        options = _a.options;
      var _b = _this.state,
        selectedOptions = _b.selectedOptions,
        isOpen = _b.isOpen;
      var filteredOptions = _this.state.filteredOptions;
      var canMoreOptionsBeSelected =
        maxSelections != null ? selectedOptions.length < maxSelections : true;
      var areFilteredOptionsAvailable =
        filteredOptions && filteredOptions.length > 0;
      var skipValueSelection = false;
      if (
        !maxSelections ||
        (canMoreOptionsBeSelected && areFilteredOptionsAvailable)
      ) {
        if (!selectedOption || !selectedOption.length) return;
        var option = lodash_1['default'].isString(selectedOption)
          ? selectedOption.trim()
          : selectedOption.filter(function (item) {
              return item;
            });
        var newSelectedOptions_1 = __spreadArray([], selectedOptions, true);
        if (option && Array.isArray(option)) {
          filteredOptions = options;
          option.forEach(function (item) {
            var optionCanBeSelected =
              (multipleSameSelections && filteredOptions.includes(item)) ||
              (filteredOptions.includes(item) &&
                !selectedOptions.includes(item) &&
                !newSelectedOptions_1.includes(item));
            if (!optionCanBeSelected && !skipValueSelection) {
              _this._setInputValue(item, true);
              skipValueSelection = true;
              return;
            }
            if (
              item &&
              optionCanBeSelected &&
              isOpen &&
              !skipValueSelection &&
              newSelectedOptions_1.length < maxSelections
            ) {
              newSelectedOptions_1.push(item);
            }
          });
        } else {
          var optionCanBeSelected =
            multipleSameSelections || !selectedOptions.includes(option);
          if (option && optionCanBeSelected && isOpen) {
            newSelectedOptions_1.push(option);
          }
        }
        _this.selectionChanged(newSelectedOptions_1, event);
        _this.setState({
          selectedOptions: newSelectedOptions_1,
          isOpen: false,
        });
      }
      if (!skipValueSelection) {
        _this._setInputValue('');
      }
    };
    _this.removeOption = function (index, event) {
      var selectedOptions = _this.state.selectedOptions;
      lodash_1['default'].pullAt(selectedOptions, index);
      _this.selectionChanged(selectedOptions, event);
      _this.setState({
        selectedOptions: selectedOptions,
      });
    };
    _this.selectionChanged = function (selectedOptions, event) {
      if (_this.props.onChange) _this.props.onChange(selectedOptions, event);
    };
    // returns an object containing props, theme, and method handlers
    // associated with rendering this.state.selectedOptions, the user can call
    // this in the body of the renderSelections function
    _this.getSelectionProps = function (_a) {
      var _b = _a === void 0 ? {} : _a,
        removeSelection = _b.removeSelection;
      var themeId = _this.props.themeId;
      var _c = _this.state,
        inputValue = _c.inputValue,
        isOpen = _c.isOpen,
        selectedOptions = _c.selectedOptions,
        composedTheme = _c.composedTheme;
      return {
        inputValue: inputValue,
        isOpen: isOpen,
        selectedOptions: selectedOptions,
        theme: composedTheme[themeId],
        removeSelection: function (
          index,
          event // the user's custom removeSelection event handler is composed with
        ) {
          // the internal functionality of Autocomplete (this.removeOption)
          return (0, props_1.composeFunctions)(
            removeSelection,
            _this.removeOption
          )(index, event);
        },
      };
    };
    // ======== PRIVATE METHOD ==========
    _this._removeOptions = function () {
      var onChange = _this.props.onChange;
      onChange ? onChange([]) : null;
      _this.setState({
        selectedOptions: [],
        inputValue: '',
      });
    };
    _this._filterOptions = function (value) {
      var filteredOptions = [];
      if (value !== '') {
        lodash_1['default'].some(_this.props.options, function (option) {
          if (lodash_1['default'].startsWith(option, value)) {
            filteredOptions.push(option);
          }
        });
      } else {
        filteredOptions = _this.props.options;
      }
      return filteredOptions;
    };
    _this._filterInvalidChars = function (value) {
      var filteredValue = '';
      if (_this.props.invalidCharsRegex.test(value)) {
        filteredValue = value.replace(_this.props.invalidCharsRegex, '');
      } else {
        filteredValue = value;
      }
      return filteredValue;
    };
    _this._setInputValue = function (value, shouldFocus) {
      var multipleValues = value.split(' ');
      if (multipleValues && multipleValues.length > 1) {
        var selectedOptions_1 = [];
        multipleValues.forEach(function (itemValue) {
          var filteredValue = _this._filterInvalidChars(itemValue);
          selectedOptions_1 = __spreadArray(
            __spreadArray([], selectedOptions_1, true),
            _this._filterOptions(filteredValue),
            true
          );
        });
        _this.setState({
          isOpen: true,
          inputValue: '',
          filteredOptions: Array.from(new Set(selectedOptions_1)),
        });
      } else {
        var filteredValue = _this._filterInvalidChars(value);
        var filteredOptions = _this._filterOptions(filteredValue);
        _this.setState({
          isOpen: !!value,
          inputValue: filteredValue,
          filteredOptions: filteredOptions,
        });
        setTimeout(function () {
          if (shouldFocus) _this.focus();
        }, 0);
      }
    };
    // define refs
    _this.rootElement = react_1['default'].createRef();
    _this.inputElement = react_1['default'].createRef();
    _this.suggestionsElement = react_1['default'].createRef();
    _this.optionsElement = react_1['default'].createRef();
    var context = props.context,
      themeId = props.themeId,
      theme = props.theme,
      themeOverrides = props.themeOverrides,
      sortAlphabetically = props.sortAlphabetically,
      options = props.options,
      preselectedOptions = props.preselectedOptions;
    _this.state = {
      inputValue: '',
      error: '',
      selectedOptions: preselectedOptions || [],
      filteredOptions:
        sortAlphabetically && options ? options.sort() : options || [],
      isOpen: false,
      mouseIsOverOptions: false,
      composedTheme: (0, themes_1.composeTheme)(
        (0, themes_1.addThemeId)(theme || context.theme, themeId),
        (0, themes_1.addThemeId)(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
    };
    return _this;
  }
  AutocompleteBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  AutocompleteBase.prototype.render = function () {
    var _this = this;
    // destructuring props ensures only the "...rest" get passed down
    var _a = this.props,
      context = _a.context,
      skin = _a.skin,
      error = _a.error,
      rest = __rest(_a, ['context', 'skin', 'error']);
    var AutocompleteSkin = skin || context.skins[_1.IDENTIFIERS.AUTOCOMPLETE];
    return (
      <GlobalListeners_1.GlobalListeners
        mouseIsOverOptions={this.state.mouseIsOverOptions}
        optionsIsOpen={this.state.isOpen}
        optionsIsOpeningUpward={this.props.isOpeningUpward}
        optionsRef={this.optionsElement}
        rootRef={this.rootElement}
        toggleOpen={this.toggleOpen}
      >
        {function (_a) {
          var optionsMaxHeight = _a.optionsMaxHeight,
            optionHeight = _a.optionHeight;
          return (
            <AutocompleteSkin
              error={error || _this.state.error}
              filteredOptions={_this.state.filteredOptions}
              getSelectionProps={_this.getSelectionProps}
              handleAutocompleteClick={_this.handleAutocompleteClick}
              handleChange={_this.handleChange}
              handleInputChange={_this.handleInputChange}
              inputRef={_this.inputElement}
              inputValue={_this.state.inputValue}
              isOpen={_this.state.isOpen}
              onKeyDown={_this.onKeyDown}
              optionsMaxHeight={optionsMaxHeight}
              optionsRef={_this.optionsElement}
              removeOption={_this.removeOption}
              rootRef={_this.rootElement}
              selectedOptions={_this.state.selectedOptions}
              suggestionsRef={_this.suggestionsElement}
              theme={_this.state.composedTheme}
              toggleMouseLocation={_this.toggleMouseLocation}
              toggleOpen={_this.toggleOpen}
              optionHeight={optionHeight}
              {...rest}
            />
          );
        }}
      </GlobalListeners_1.GlobalListeners>
    );
  };
  // define static properties
  AutocompleteBase.displayName = 'Autocomplete';
  AutocompleteBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    error: null,
    invalidCharsRegex: /[^a-zA-Z0-9\s]/g,
    // only allow letters and numbers by default
    isOpeningUpward: false,
    maxVisibleOptions: 10,
    // max number of visible options
    multipleSameSelections: true,
    // if true then same word can be selected multiple times
    options: [],
    requiredSelections: [],
    sortAlphabetically: true,
    // options are sorted alphabetically by default
    theme: null,
    themeId: _1.IDENTIFIERS.AUTOCOMPLETE,
    themeOverrides: {},
  };
  return AutocompleteBase;
})(react_1.Component);
exports.Autocomplete = (0, withTheme_1.withTheme)(AutocompleteBase);
