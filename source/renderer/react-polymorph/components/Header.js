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
const __spreadArray =
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
exports.Header = void 0;
// @ts-nocheck
const react_1 = require('react');
const lodash_1 = require('lodash');
// utility functions
const withTheme_1 = require('./HOC/withTheme');
const themes_1 = require('../utils/themes');
// constants
const _1 = require('.');

const HeaderBase = /** @class */ (function (_super) {
  __extends(HeaderBase, _super);
  function HeaderBase(props) {
    const _this = _super.call(this, props) || this;
    _this._assembleInlineStyles = function (_a) {
      const { center } = _a;
      const { lowerCase } = _a;
      const { left } = _a;
      const { right } = _a;
      const { upperCase } = _a;
      const inlineStyles = {};
      const textAlign = (0, lodash_1.pickBy)({
        center,
        left,
        right,
      });
      const textTransform = (0, lodash_1.pickBy)({
        lowerCase,
        upperCase,
      });
      if (!(0, lodash_1.isEmpty)(textAlign)) {
        inlineStyles.textAlign = Object.keys(textAlign)[0];
      }
      if (!(0, lodash_1.isEmpty)(textTransform)) {
        inlineStyles.textTransform = Object.keys(textTransform)[0];
      }
      return inlineStyles;
    };
    _this._assembleHeaderTheme = function (styleProps) {
      const activeClasses = _this._getActiveClasses(styleProps);
      const theme = _this.state.composedTheme[_this.props.themeId];
      return activeClasses.reduce((reducedTheme, activeClass) => {
        if (activeClass && Object.hasOwnProperty.call(theme, activeClass)) {
          reducedTheme[activeClass] = theme[activeClass];
        }
        return reducedTheme;
      }, {});
    };
    _this._getActiveFont = function (_a) {
      const { light } = _a;
      const { medium } = _a;
      const { regular } = _a;
      const { thin } = _a;
      const { bold } = _a;
      const fontProps = (0, lodash_1.pickBy)({
        light,
        medium,
        regular,
        thin,
        bold,
      });
      if ((0, lodash_1.isEmpty)(fontProps)) {
        return;
      }
      // returns the first active font if more than 1 is passed
      return Object.keys(fontProps)[0];
    };
    _this._getActiveTheme = function (_a) {
      const { h1 } = _a;
      const { h2 } = _a;
      const { h3 } = _a;
      const { h4 } = _a;
      const themeProps = (0, lodash_1.pickBy)({
        h1,
        h2,
        h3,
        h4,
      });
      if ((0, lodash_1.isEmpty)(themeProps)) {
        return;
      }
      // returns the first active theme if more than 1 is passed
      return Object.keys(themeProps)[0];
    };
    _this._getActiveClasses = function (styleProps) {
      const activeClasses = ['header'];
      const activeTheme = _this._getActiveTheme(styleProps);
      const activeFont = _this._getActiveFont(styleProps);
      if (activeTheme) {
        return __spreadArray(
          __spreadArray([], activeClasses, true),
          [activeTheme],
          false
        );
      }
      if (activeFont) {
        return __spreadArray(
          __spreadArray([], activeClasses, true),
          [activeFont],
          false
        );
      }
      return __spreadArray(
        __spreadArray([], activeClasses, true),
        [activeTheme, activeFont],
        false
      ).filter((val) => {
        return val;
      });
    };
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
    };
    return _this;
  }
  HeaderBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  HeaderBase.prototype.render = function () {
    const _a = this.props;
    const { children } = _a;
    const { className } = _a;
    const { skin } = _a;
    const { context } = _a;
    const styleProps = __rest(_a, ['children', 'className', 'skin', 'context']);
    const HeaderSkin = skin || context.skins[_1.IDENTIFIERS.HEADER];
    const reducedTheme = this._assembleHeaderTheme(styleProps);
    const inlineStyles = this._assembleInlineStyles(styleProps);
    return (
      <HeaderSkin
        className={className}
        inlineStyles={inlineStyles}
        theme={reducedTheme}
      >
        {children}
      </HeaderSkin>
    );
  };
  // define static properties
  HeaderBase.displayName = 'Header';
  HeaderBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    theme: null,
    themeId: _1.IDENTIFIERS.HEADER,
    themeOverrides: {},
  };
  return HeaderBase;
})(react_1.Component);
exports.Header = (0, withTheme_1.withTheme)(HeaderBase);
