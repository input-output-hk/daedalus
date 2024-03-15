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
exports.Link = void 0;
// @ts-nocheck
var react_1 = require('react');
// internal utility functions
var withTheme_1 = require('./HOC/withTheme');
var themes_1 = require('../utils/themes');
// import constants
var _1 = require('.');
var LinkBase = /** @class */ (function (_super) {
  __extends(LinkBase, _super);
  function LinkBase(props) {
    var _this = _super.call(this, props) || this;
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
  LinkBase.prototype.componentDidUpdate = function (prevProps) {
    if (prevProps !== this.props) {
      (0, themes_1.didThemePropsChange)(
        prevProps,
        this.props,
        this.setState.bind(this)
      );
    }
  };
  LinkBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    var _a = this.props,
      skin = _a.skin,
      context = _a.context,
      rest = __rest(_a, ['skin', 'context']);
    var LinkSkin = skin || context.skins[this.props.themeId];
    return <LinkSkin theme={this.state.composedTheme} {...rest} />;
  };
  // define static properties
  LinkBase.displayName = 'Link';
  LinkBase.defaultProps = {
    context: (0, withTheme_1.createEmptyContext)(),
    theme: null,
    themeId: _1.IDENTIFIERS.LINK,
    themeOverrides: {},
    hasIconBefore: false,
    hasIconAfter: true,
    isUnderlined: true,
    underlineOnHover: false,
  };
  return LinkBase;
})(react_1.Component);
exports.Link = (0, withTheme_1.withTheme)(LinkBase);
