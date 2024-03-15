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
exports.__esModule = true;
exports.ThemeProvider = void 0;
// @ts-nocheck
const react_1 = require('react');
// external libraries
const lodash_1 = require('lodash');
// contains default theme and context provider
const ThemeContext_1 = require('./HOC/ThemeContext');
// imports the Root Theme API object which specifies the shape
// of a complete theme for every component in this library, used in this.composeLibraryTheme
const API_1 = require('../themes/API');
// internal utility functions
const themes_1 = require('../utils/themes');
const props_1 = require('../utils/props');
const ThemeVariablesProvider_1 = require('./ThemeVariablesProvider');

const ThemeProvider = /** @class */ (function (_super) {
  __extends(ThemeProvider, _super);
  function ThemeProvider(props) {
    const _this = _super.call(this, props) || this;
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
      const composedTheme = {};
      for (const componentName in API_1.ROOT_THEME_API) {
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
      const composedComponentTheme = (0, lodash_1.cloneDeep)(componentThemeAPI);
      for (const className in componentThemeAPI) {
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
    const { theme } = props;
    const { themeOverrides } = props;
    _this.state = {
      theme: _this._composeLibraryTheme(theme, themeOverrides),
    };
    return _this;
  }
  ThemeProvider.prototype.componentDidUpdate = function (prevProps) {
    const _this = this;
    const prevTheme = prevProps.theme;
    const prevThemeOverrides = prevProps.themeOverrides;
    const _a = this.props;
    const { theme } = _a;
    const { themeOverrides } = _a;
    if (
      !(0, lodash_1.isEqual)(prevTheme, theme) ||
      !(0, lodash_1.isEqual)(prevThemeOverrides, themeOverrides)
    ) {
      this.setState(() => {
        return {
          theme: _this._composeLibraryTheme(theme, themeOverrides),
        };
      });
    }
  };
  ThemeProvider.prototype.render = function () {
    const { theme } = this.state;
    const _a = this.props;
    const { isRoot } = _a;
    const { skins } = _a;
    const { variables } = _a;
    const providerState = {
      skins,
      theme,
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
