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
exports.__esModule = true;
exports.Select = void 0;
// @ts-nocheck
var react_1 = require('react');
// internal components
var GlobalListeners_1 = require('./HOC/GlobalListeners');
// internal utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
// import constants
var _1 = require('.');
var SelectBase = /** @class */ (function (_super) {
  __extends(SelectBase, _super);
  function SelectBase(props) {
    var _this = _super.call(this, props) || this;
    // ========= PUBLIC SKIN API =========
    // applying focus to the input element will
    // toggle options open because Select's input is read only
    _this.focus = function () {
      return _this.toggleOpen();
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
      _this.setState({
        isOpen: !_this.state.isOpen,
      });
    };
    _this.toggleMouseLocation = function () {
      return _this.setState({
        mouseIsOverOptions: !_this.state.mouseIsOverOptions,
      });
    };
    _this.handleInputClick = function (event) {
      event.stopPropagation();
      event.preventDefault();
      var inputElement = _this.inputElement;
      if (
        inputElement.current &&
        document.activeElement === inputElement.current
      ) {
        inputElement.current.blur();
      }
      _this.toggleOpen();
    };
    _this.handleChange = function (option, event) {
      // check if the user passed an onChange handler and call it
      if (_this.props.onChange) _this.props.onChange(option.value, event);
      // onChange is called when an option is selected, so close options
      _this.toggleOpen();
    };
    _this.getSelectedOption = function () {
      var _a = _this.props,
        options = _a.options,
        value = _a.value,
        allowBlank = _a.allowBlank;
      for (var _i = 0, options_1 = options; _i < options_1.length; _i++) {
        var option = options_1[_i];
        if (option.value === value) return option;
      }
      if (!allowBlank) return options[0];
    };
    // define ref
    _this.rootElement = react_1['default'].createRef();
    _this.inputElement = react_1['default'].createRef();
    _this.optionsElement = react_1['default'].createRef();
    var context = props.context,
      themeId = props.themeId,
      theme = props.theme,
      themeOverrides = props.themeOverrides;
    _this.state = {
      composedTheme: (0, themes_1.composeTheme)(
        (0, themes_1.addThemeId)(theme || context.theme, themeId),
        (0, themes_1.addThemeId)(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
      isOpen: false,
      mouseIsOverOptions: false,
    };
    return _this;
  }
  SelectBase.prototype.componentDidMount = function () {
    // check for autoFocus of input element
    if (this.props.autoFocus) {
      return this.focus();
    }
  };
  SelectBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  SelectBase.prototype.render = function () {
    var _this = this;
    // destructuring props ensures only the "...rest" get passed down
    var _a = this.props,
      skin = _a.skin,
      context = _a.context,
      optionHeight = _a.optionHeight,
      searchHeight = _a.searchHeight,
      rest = __rest(_a, ['skin', 'context', 'optionHeight', 'searchHeight']);
    var SelectSkin = skin || context.skins[_1.IDENTIFIERS.SELECT];
    return (
      <GlobalListeners_1.GlobalListeners
        mouseIsOverOptions={this.state.mouseIsOverOptions}
        optionsIsOpen={this.state.isOpen}
        optionsIsOpeningUpward={this.props.isOpeningUpward}
        optionsRef={this.optionsElement}
        rootRef={this.rootElement}
        toggleOpen={this.toggleOpen}
        hasSearch={this.props.hasSearch}
        optionsLength={this.props.options.length}
      >
        {function (_a) {
          var optionsMaxHeight = _a.optionsMaxHeight;
          return (
            <SelectSkin
              isOpen={_this.state.isOpen}
              rootRef={_this.rootElement}
              inputRef={_this.inputElement}
              optionsRef={_this.optionsElement}
              optionsMaxHeight={optionsMaxHeight}
              theme={_this.state.composedTheme}
              getSelectedOption={_this.getSelectedOption}
              handleInputClick={_this.handleInputClick}
              handleChange={_this.handleChange}
              toggleOpen={_this.toggleOpen}
              toggleMouseLocation={_this.toggleMouseLocation}
              optionHeight={optionHeight}
              searchHeight={searchHeight}
              {...rest}
            />
          );
        }}
      </GlobalListeners_1.GlobalListeners>
    );
  };
  // define static properties
  SelectBase.displayName = 'Select';
  SelectBase.defaultProps = {
    allowBlank: true,
    autoFocus: false,
    context: (0, withTheme_1.createEmptyContext)(),
    isOpeningUpward: false,
    options: [],
    theme: null,
    themeOverrides: {},
    themeId: _1.IDENTIFIERS.SELECT,
    value: '',
  };
  return SelectBase;
})(react_1.Component);
exports.Select = (0, withTheme_1.withTheme)(SelectBase);
