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
exports.Header = void 0;
// @ts-nocheck
var react_1 = require('react');
var lodash_1 = require('lodash');
// utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
// constants
var _1 = require('.');
var HeaderBase = /** @class */ (function (_super) {
  __extends(HeaderBase, _super);
  function HeaderBase(props) {
    var _this = _super.call(this, props) || this;
    _this._assembleInlineStyles = function (_a) {
      var center = _a.center,
        lowerCase = _a.lowerCase,
        left = _a.left,
        right = _a.right,
        upperCase = _a.upperCase;
      var inlineStyles = {};
      var textAlign = (0, lodash_1.pickBy)({
        center: center,
        left: left,
        right: right,
      });
      var textTransform = (0, lodash_1.pickBy)({
        lowerCase: lowerCase,
        upperCase: upperCase,
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
      var activeClasses = _this._getActiveClasses(styleProps);
      var theme = _this.state.composedTheme[_this.props.themeId];
      return activeClasses.reduce(function (reducedTheme, activeClass) {
        if (activeClass && Object.hasOwnProperty.call(theme, activeClass)) {
          reducedTheme[activeClass] = theme[activeClass];
        }
        return reducedTheme;
      }, {});
    };
    _this._getActiveFont = function (_a) {
      var light = _a.light,
        medium = _a.medium,
        regular = _a.regular,
        thin = _a.thin,
        bold = _a.bold;
      var fontProps = (0, lodash_1.pickBy)({
        light: light,
        medium: medium,
        regular: regular,
        thin: thin,
        bold: bold,
      });
      if ((0, lodash_1.isEmpty)(fontProps)) {
        return;
      }
      // returns the first active font if more than 1 is passed
      return Object.keys(fontProps)[0];
    };
    _this._getActiveTheme = function (_a) {
      var h1 = _a.h1,
        h2 = _a.h2,
        h3 = _a.h3,
        h4 = _a.h4;
      var themeProps = (0, lodash_1.pickBy)({
        h1: h1,
        h2: h2,
        h3: h3,
        h4: h4,
      });
      if ((0, lodash_1.isEmpty)(themeProps)) {
        return;
      }
      // returns the first active theme if more than 1 is passed
      return Object.keys(themeProps)[0];
    };
    _this._getActiveClasses = function (styleProps) {
      var activeClasses = ['header'];
      var activeTheme = _this._getActiveTheme(styleProps);
      var activeFont = _this._getActiveFont(styleProps);
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
      ).filter(function (val) {
        return val;
      });
    };
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
    var _a = this.props,
      children = _a.children,
      className = _a.className,
      skin = _a.skin,
      context = _a.context,
      styleProps = __rest(_a, ['children', 'className', 'skin', 'context']);
    var HeaderSkin = skin || context.skins[_1.IDENTIFIERS.HEADER];
    var reducedTheme = this._assembleHeaderTheme(styleProps);
    var inlineStyles = this._assembleInlineStyles(styleProps);
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
