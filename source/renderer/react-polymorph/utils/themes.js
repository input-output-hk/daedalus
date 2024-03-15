'use strict';
exports.__esModule = true;
exports.didThemePropsChange = exports.composeTheme = exports.addThemeId = exports.composeComponentStyles = exports.appendToProperty = void 0;
var lodash_1 = require('lodash');
var props_1 = require('./props');
var appendToProperty = function (dest, name, value) {
  dest[name] === '' ? (dest[name] = value) : (dest[name] += ' '.concat(value));
};
exports.appendToProperty = appendToProperty;
var composeComponentStyles = function (componentStyles, componentTheme) {
  if (!componentTheme) return;
  for (var property in componentStyles) {
    if ((0, props_1.hasProperty)(componentStyles, property)) {
      if ((0, props_1.hasProperty)(componentTheme, property)) {
        (0, exports.appendToProperty)(
          componentStyles,
          property,
          componentTheme[property]
        );
      }
    }
  }
};
exports.composeComponentStyles = composeComponentStyles;
// checks for the existence of a property on theme
// that matches the value of themeId (string)
// if the property exists, also checks the type of
// theme[themeId] to ensure it's an object
var addThemeId = function (theme, themeId) {
  var _a;
  if (theme === void 0) {
    theme = {};
  }
  if (theme && !(0, lodash_1.isEmpty)(theme) && themeId) {
    var themeIdExists = (0, props_1.hasProperty)(theme, themeId);
    var themeIdIsObj = typeof theme[themeId] === 'object';
    return themeIdExists && themeIdIsObj
      ? theme
      : ((_a = {}), (_a[themeId] = theme), _a);
  }
  return theme;
};
exports.addThemeId = addThemeId;
/**
 * Composes a base theme with the given overrides, which should
 * be provided in the same schema, defined by the theme API param.
 *
 * @param theme - The base theme to be composed with overrides
 * @param themeOverrides - The custom overrides for the base theme
 * @param themeAPI - The theme API schema that should be used for composition
 * @returns {{}} - The composed theme
 */
var composeTheme = function (theme, themeOverrides, themeAPI) {
  if (theme === void 0) {
    theme = {};
  }
  if (themeOverrides === void 0) {
    themeOverrides = {};
  }
  if (themeAPI === void 0) {
    themeAPI = {};
  }
  // Return theme if there are no overrides provided
  if ((0, lodash_1.isEmpty)(themeOverrides)) return theme;
  // final object to be returned
  var composedTheme = (0, lodash_1.cloneDeep)(themeAPI);
  for (var componentId in themeAPI) {
    if ((0, props_1.hasProperty)(composedTheme, componentId)) {
      var componentStyles = composedTheme[componentId];
      (0, exports.composeComponentStyles)(componentStyles, theme[componentId]);
      (0, exports.composeComponentStyles)(
        componentStyles,
        themeOverrides[componentId]
      );
    }
  }
  return composedTheme;
};
exports.composeTheme = composeTheme;
// Used in componentDidUpdate, this function compares the current
// set of theme related props against the next set to see if any have changed.
// If true, a component's theme is recomposed and local state is updated
var didThemePropsChange = function (_a, _b, setState) {
  var context = _a.context,
    themeId = _a.themeId,
    theme = _a.theme,
    themeOverrides = _a.themeOverrides;
  var nextContext = _b.context,
    nextThemeId = _b.themeId,
    nextTheme = _b.theme,
    nextOverrides = _b.themeOverrides;
  if (
    !(0, lodash_1.isEqual)(context, nextContext) ||
    !(0, lodash_1.isEqual)(themeId, nextThemeId) ||
    !(0, lodash_1.isEqual)(theme, nextTheme) ||
    !(0, lodash_1.isEqual)(themeOverrides, nextOverrides)
  ) {
    setState(function () {
      return {
        composedTheme: (0, exports.composeTheme)(
          (0, exports.addThemeId)(nextTheme || nextContext.theme, nextThemeId),
          (0, exports.addThemeId)(nextOverrides, nextThemeId),
          nextContext.ROOT_THEME_API
        ),
      };
    });
  }
};
exports.didThemePropsChange = didThemePropsChange;
