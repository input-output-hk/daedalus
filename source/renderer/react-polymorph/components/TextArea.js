const __extends =
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
          for (const p in b)
            if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p];
        };
      return extendStatics(d, b);
    };
    return function (d, b) {
      if (typeof b !== 'function' && b !== null)
        throw new TypeError(
          `Class extends value ${String(b)} is not a constructor or null`
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
const __rest =
  (this && this.__rest) ||
  function (s, e) {
    const t = {};
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
exports.TextArea = void 0;
// @ts-nocheck
const react_1 = require('react');
// external libraries
const lodash_1 = require('lodash');
// internal utility functions
const withTheme_1 = require('./HOC/withTheme');
const themes_1 = require('../utils/themes');
// import constants
const _1 = require('.');

const TextAreaBase = /** @class */ (function (_super) {
  __extends(TextAreaBase, _super);
  function TextAreaBase(props) {
    const _this = _super.call(this, props) || this;
    _this.focus = function () {
      const { textareaElement } = _this;
      if (!textareaElement.current) return;
      textareaElement.current.focus();
    };
    _this.onChange = function (event) {
      const _a = _this.props;
      const { onChange } = _a;
      const { disabled } = _a;
      if (disabled) return;
      if (onChange) onChange(_this._processValue(event.target.value), event);
    };
    _this._setError = function (error) {
      return _this.setState({
        error,
      });
    };
    // define ref
    _this.textareaElement = react_1.default.createRef();
    const { context } = props;
    const { themeId } = props;
    const { theme } = props;
    const { themeOverrides } = props;
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
  TextAreaBase.prototype.componentDidMount = function () {
    const _a = this.props;
    const { autoResize } = _a;
    const { autoFocus } = _a;
    if (autoResize) {
      window.addEventListener('resize', this._handleAutoresize);
      this._handleAutoresize();
    }
    if (autoFocus) {
      this.focus();
    }
  };
  TextAreaBase.prototype.componentDidUpdate = function (prevProps) {
    if (this.props.autoResize) this._handleAutoresize();
    if (prevProps !== this.props) {
      if (!prevProps.autoResize && this.props.autoResize) {
        window.addEventListener('resize', this._handleAutoresize);
      } else if (prevProps.autoResize && !this.props.autoResize) {
        window.removeEventListener('resize', this._handleAutoresize);
      }
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  TextAreaBase.prototype.componentWillUnmount = function () {
    if (this.props.autoResize) {
      window.removeEventListener('resize', this._handleAutoresize);
    }
  };
  TextAreaBase.prototype._processValue = function (value) {
    return (0, lodash_1.flow)([
      this._enforceStringValue,
      this._enforceMaxLength,
      this._enforceMinLength,
    ]).call(this, value);
  };
  TextAreaBase.prototype._enforceStringValue = function (value) {
    if (!(0, lodash_1.isString)(value)) {
      throw new Error('Values passed to TextArea::onChange must be strings');
    }
    return value;
  };
  TextAreaBase.prototype._enforceMaxLength = function (value) {
    const { maxLength } = this.props;
    const isTooLong = maxLength != null && value.length > maxLength;
    return isTooLong ? value.substring(0, maxLength) : value;
  };
  TextAreaBase.prototype._enforceMinLength = function (value) {
    const { minLength } = this.props;
    const isTooShort = minLength != null && value.length < minLength;
    if (isTooShort) {
      this._setError('Please enter a valid input');
    } else if (this.state.error !== '') {
      this._setError('');
    }
    return value;
  };
  TextAreaBase.prototype._handleAutoresize = function () {
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
    textareaElement.current.style.height = ''.concat(
      textareaElement.current.scrollHeight + heightOffset,
      'px'
    );
  };
  TextAreaBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    const _a = this.props;
    const { skin } = _a;
    const { error } = _a;
    const { context } = _a;
    const rest = __rest(_a, ['skin', 'error', 'context']);
    const TextAreaSkin = skin || context.skins[_1.IDENTIFIERS.TEXT_AREA];
    return (
      <TextAreaSkin
        error={error || this.state.error}
        onChange={this.onChange}
        textareaRef={this.textareaElement}
        theme={this.state.composedTheme}
        {...rest}
      />
    );
  };
  // define static properties
  TextAreaBase.displayName = 'TextArea';
  TextAreaBase.defaultProps = {
    autoFocus: false,
    autoResize: true,
    context: (0, withTheme_1.createEmptyContext)(),
    theme: null,
    themeId: _1.IDENTIFIERS.TEXT_AREA,
    themeOverrides: {},
    value: '',
  };
  return TextAreaBase;
})(react_1.Component);
exports.TextArea = (0, withTheme_1.withTheme)(TextAreaBase);
