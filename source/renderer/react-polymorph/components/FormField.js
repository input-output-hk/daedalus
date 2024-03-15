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
exports.FormField = void 0;
// @ts-nocheck
const react_1 = require('react');
// internal utility functions
const withTheme_1 = require('./HOC/withTheme');
const themes_1 = require('../utils/themes');
// import constants
const _1 = require('.');

const FormFieldBase = /** @class */ (function (_super) {
  __extends(FormFieldBase, _super);
  function FormFieldBase(props) {
    let _this = this;
    let _a;
    _this = _super.call(this, props) || this;
    _this.setError = function (error) {
      return _this.setState({
        error,
      });
    };
    _this.focusChild = function () {
      const { formFieldRef } = _this;
      if (formFieldRef && formFieldRef.current) {
        if (typeof formFieldRef.current.focus === 'function') {
          formFieldRef.current.focus();
        }
      }
    };
    const { context } = props;
    const { themeId } = props;
    const { theme } = props;
    const { themeOverrides } = props;
    _this.formFieldRef =
      (_a = props.formFieldRef) !== null && _a !== void 0
        ? _a
        : react_1.default.createRef();
    _this.state = {
      error: '',
      composedTheme: (0, themes_1.composeTheme)(
        (0, themes_1.addThemeId)(theme || context.theme, themeId),
        (0, themes_1.addThemeId)(themeOverrides, themeId),
        context.ROOT_THEME_API
      ),
    };
    return _this;
  }
  FormFieldBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  FormFieldBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    const _a = this.props;
    const { skin } = _a;
    const { theme } = _a;
    const { themeOverrides } = _a;
    const { error } = _a;
    const { context } = _a;
    const rest = __rest(_a, [
      'skin',
      'theme',
      'themeOverrides',
      'error',
      'context',
    ]);
    const FormFieldSkin = skin || context.skins[_1.IDENTIFIERS.FORM_FIELD];
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
  };
  // define static properties
  FormFieldBase.displayName = 'FormField';
  FormFieldBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    isShowingErrorOnFocus: true,
    isShowingErrorOnHover: true,
    theme: null,
    themeId: _1.IDENTIFIERS.FORM_FIELD,
    themeOverrides: {},
  };
  return FormFieldBase;
})(react_1.Component);
exports.FormField = (0, withTheme_1.withTheme)(FormFieldBase);
