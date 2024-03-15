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
exports.Input = void 0;
// @ts-nocheck
var react_1 = require('react');
// external libraries
var lodash_1 = require('lodash');
// utilities
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
// constants
var _1 = require('.');
var InputBase = /** @class */ (function (_super) {
  __extends(InputBase, _super);
  function InputBase(props) {
    var _this = this;
    var _a;
    _this = _super.call(this, props) || this;
    _this.onChange = function (event) {
      var _a = _this.props,
        onChange = _a.onChange,
        disabled = _a.disabled,
        readOnly = _a.readOnly;
      if (disabled || readOnly) return;
      if (onChange) onChange(_this._processValue(event.target.value), event);
    };
    _this.focus = function () {
      var inputElement = _this.inputElement;
      if (!inputElement.current) return;
      inputElement.current.focus();
    };
    _this._setError = function (error) {
      var setError = _this.props.setError;
      // checks for setError func from FormField component
      // if this Input instance is being used within the render function
      // of a FormField instance, the error field within FormField's local state
      // will be set
      if (setError) setError(error);
      _this.setState({
        error: error,
      });
    };
    _this.inputElement =
      (_a = props.inputRef) !== null && _a !== void 0
        ? _a
        : react_1['default'].createRef();
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
      error: '',
    };
    return _this;
  }
  InputBase.prototype.componentDidMount = function () {
    if (this.props.autoFocus) this.focus();
  };
  InputBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  InputBase.prototype._processValue = function (value) {
    return (0, lodash_1.flow)([
      this._enforceStringValue,
      this._enforceMaxLength,
      this._enforceMinLength,
    ]).call(this, value);
  };
  InputBase.prototype._enforceStringValue = function (value) {
    if (!(0, lodash_1.isString)(value)) {
      throw new Error('Values passed to Input::onChange must be strings');
    }
    return value;
  };
  InputBase.prototype._enforceMaxLength = function (value) {
    var maxLength = this.props.maxLength;
    var isTooLong = maxLength != null && value.length > maxLength;
    return isTooLong ? value.substring(0, maxLength) : value;
  };
  InputBase.prototype._enforceMinLength = function (value) {
    var minLength = this.props.minLength;
    var isTooShort = minLength != null && value.length < minLength;
    if (isTooShort) {
      this._setError('Please enter a valid input');
    } else if (this.state.error !== '') {
      this._setError('');
    }
    return value;
  };
  InputBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    var _a = this.props,
      skin = _a.skin,
      context = _a.context,
      theme = _a.theme,
      themeOverrides = _a.themeOverrides,
      onChange = _a.onChange,
      error = _a.error,
      maxLength = _a.maxLength,
      minLength = _a.minLength,
      setError = _a.setError,
      autoFocus = _a.autoFocus,
      rest = __rest(_a, [
        'skin',
        'context',
        'theme',
        'themeOverrides',
        'onChange',
        'error',
        'maxLength',
        'minLength',
        'setError',
        'autoFocus',
      ]);
    var InputSkin = skin || context.skins[_1.IDENTIFIERS.INPUT];
    return (
      <InputSkin
        error={error || this.state.error}
        onChange={this.onChange}
        inputRef={this.inputElement}
        theme={this.state.composedTheme}
        {...rest}
      />
    );
  };
  InputBase.displayName = 'Input';
  InputBase.defaultProps = {
    autoFocus: false,
    context: (0, withTheme_1.createEmptyContext)(),
    error: '',
    isShowingErrorOnFocus: true,
    isShowingErrorOnHover: true,
    readOnly: false,
    theme: null,
    themeId: _1.IDENTIFIERS.INPUT,
    themeOverrides: {},
    value: '',
  };
  return InputBase;
})(react_1.Component);
exports.Input = (0, withTheme_1.withTheme)(InputBase);
