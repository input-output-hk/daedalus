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
exports.__esModule = true;
exports.ThemeProvider = void 0;
// @ts-nocheck
var react_1 = require('react');
// external libraries
var lodash_1 = require('lodash');
// contains default theme and context provider
var ThemeContext_1 = require('./HOC/ThemeContext');
// imports the Root Theme API object which specifies the shape
// of a complete theme for every component in this library, used in this.composeLibraryTheme
var API_1 = require('../themes/API');
// internal utility functions
var themes_1 = require('../utils/themes');
var props_1 = require('../utils/props');
var ThemeVariablesProvider_1 = require('./ThemeVariablesProvider');
var ThemeProvider = /** @class */ (function (_super) {
  __extends(ThemeProvider, _super);
  function ThemeProvider(props) {
    var _this = _super.call(this, props) || this;
    // composeLibraryTheme returns a single obj containing theme definitions
    // for every component in the library. Every key on the returned obj is named
    // in conjunction with a component in the library and each key's value is structured
    // to contain the css definitions for each element in that component.
    // Which is just a string via CSS-Modules. Looks like this:
    // {
    //   button: { root: '', disabled: '' },
    //   input: { input: '', disabled: '', error: '' },
    //   formField: { root: '', label: '', error: '' },
    //   ... and so on, creating a complete theme for the library,
    //  }
    _this._composeLibraryTheme = function (theme, themeOverrides) {
      // if themeOverrides is empty, no need for composition
      if ((0, lodash_1.isEmpty)(themeOverrides)) {
        return theme;
      }
      // final object to be returned
      var composedTheme = {};
      for (var componentName in API_1.ROOT_THEME_API) {
        // check if ROOT_THEME_API contains the key of componentName
        if ((0, props_1.hasProperty)(API_1.ROOT_THEME_API, componentName)) {
          // check if theme contains a key of componentName
          if ((0, props_1.hasProperty)(theme, componentName)) {
            // add componentName as a key to final return obj
            composedTheme[componentName] = theme[componentName];
          }
          // also check if themeOverrides contains the key componentName
          if ((0, props_1.hasProperty)(themeOverrides, componentName)) {
            // compose theme styles with user's themeOverrides
            composedTheme[componentName] = _this._applyThemeOverrides(
              theme[componentName],
              themeOverrides[componentName],
              API_1.ROOT_THEME_API[componentName]
            );
          }
        }
      }
      return composedTheme;
    };
    _this._applyThemeOverrides = function (
      componentTheme,
      componentThemeOverrides,
      componentThemeAPI
    ) {
      // Return componentTheme if there are no overrides provided
      if ((0, lodash_1.isEmpty)(componentThemeOverrides)) {
        return componentTheme;
      }
      // final composed theme obj to be returned at end
      var composedComponentTheme = (0, lodash_1.cloneDeep)(componentThemeAPI);
      for (var className in componentThemeAPI) {
        if ((0, props_1.hasProperty)(componentThemeAPI, className)) {
          if ((0, props_1.hasProperty)(componentTheme, className)) {
            (0, themes_1.appendToProperty)(
              composedComponentTheme,
              className,
              componentTheme[className]
            );
          }
          if ((0, props_1.hasProperty)(componentThemeOverrides, className)) {
            (0, themes_1.appendToProperty)(
              composedComponentTheme,
              className,
              componentThemeOverrides[className]
            );
          }
        }
      }
      return composedComponentTheme;
    };
    var theme = props.theme,
      themeOverrides = props.themeOverrides;
    _this.state = {
      theme: _this._composeLibraryTheme(theme, themeOverrides),
    };
    return _this;
  }
  ThemeProvider.prototype.componentDidUpdate = function (prevProps) {
    var _this = this;
    var prevTheme = prevProps.theme,
      prevThemeOverrides = prevProps.themeOverrides;
    var _a = this.props,
      theme = _a.theme,
      themeOverrides = _a.themeOverrides;
    if (
      !(0, lodash_1.isEqual)(prevTheme, theme) ||
      !(0, lodash_1.isEqual)(prevThemeOverrides, themeOverrides)
    ) {
      this.setState(function () {
        return {
          theme: _this._composeLibraryTheme(theme, themeOverrides),
        };
      });
    }
  };
  ThemeProvider.prototype.render = function () {
    var theme = this.state.theme;
    var _a = this.props,
      isRoot = _a.isRoot,
      skins = _a.skins,
      variables = _a.variables;
    var providerState = {
      skins: skins,
      theme: theme,
      ROOT_THEME_API: API_1.ROOT_THEME_API,
    };
    return (
      <ThemeContext_1.ThemeContext.Provider value={providerState}>
        <ThemeVariablesProvider_1.ThemeVariablesProvider
          isRoot={isRoot}
          variables={variables}
        >
          {this.props.children}
        </ThemeVariablesProvider_1.ThemeVariablesProvider>
      </ThemeContext_1.ThemeContext.Provider>
    );
  };
  return ThemeProvider;
})(react_1.Component);
exports.ThemeProvider = ThemeProvider;
ThemeProvider.defaultProps = {
  isRoot: true,
  skins: {},
  theme: {},
  variables: {},
  themeOverrides: {},
};
