exports.__esModule = true;
exports.didThemePropsChange = exports.composeTheme = exports.addThemeId = exports.composeComponentStyles = exports.appendToProperty = void 0;
const lodash_1 = require('lodash');
const props_1 = require('./props');

const appendToProperty = function (dest, name, value) {
  dest[name] === '' ? (dest[name] = value) : (dest[name] += ' '.concat(value));
};
exports.appendToProperty = appendToProperty;
const composeComponentStyles = function (componentStyles, componentTheme) {
  if (!componentTheme) return;
  for (const property in componentStyles) {
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
const addThemeId = function (theme, themeId) {
  let _a;
  if (theme === void 0) {
    theme = {};
  }
  if (theme && !(0, lodash_1.isEmpty)(theme) && themeId) {
    const themeIdExists = (0, props_1.hasProperty)(theme, themeId);
    const themeIdIsObj = typeof theme[themeId] === 'object';
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
const composeTheme = function (theme, themeOverrides, themeAPI) {
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
  const composedTheme = (0, lodash_1.cloneDeep)(themeAPI);
  for (const componentId in themeAPI) {
    if ((0, props_1.hasProperty)(composedTheme, componentId)) {
      const componentStyles = composedTheme[componentId];
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
const didThemePropsChange = function (_a, _b, setState) {
  const { context } = _a;
  const { themeId } = _a;
  const { theme } = _a;
  const { themeOverrides } = _a;
  const nextContext = _b.context;
  const nextThemeId = _b.themeId;
  const nextTheme = _b.theme;
  const nextOverrides = _b.themeOverrides;
  if (
    !(0, lodash_1.isEqual)(context, nextContext) ||
    !(0, lodash_1.isEqual)(themeId, nextThemeId) ||
    !(0, lodash_1.isEqual)(theme, nextTheme) ||
    !(0, lodash_1.isEqual)(themeOverrides, nextOverrides)
  ) {
    setState(() => {
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
