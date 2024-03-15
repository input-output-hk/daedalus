// @flow
import { cloneDeep, isEmpty, isEqual } from 'lodash';
import { hasProperty } from './props';
import type { ThemeContextProp } from '../components/HOC/withTheme';

export const appendToProperty = (dest: {}, name: string, value: string) => {
  dest[name] === '' ? (dest[name] = value) : (dest[name] += ' ' + value);
};

export const composeComponentStyles = (
  componentStyles: {},
  componentTheme: {}
) => {
  if (!componentTheme) return;
  for (const property in componentStyles) {
    if (hasProperty(componentStyles, property)) {
      if (hasProperty(componentTheme, property)) {
        appendToProperty(componentStyles, property, componentTheme[property]);
      }
    }
  }
};

// checks for the existence of a property on theme
// that matches the value of themeId (string)
// if the property exists, also checks the type of
// theme[themeId] to ensure it's an object
export const addThemeId = (theme: Object = {}, themeId: string): Object => {
  if (theme && !isEmpty(theme) && themeId) {
    const themeIdExists = hasProperty(theme, themeId);
    const themeIdIsObj = typeof theme[themeId] === 'object';
    return themeIdExists && themeIdIsObj ? theme : { [themeId]: theme };
  }
  return theme;
};

/**
 * Composes a base theme with the given overrides, which should
 * be provided in the same schema, defined by the theme API param.
 *
 * @param theme - The base theme to be composed with overrides
 * @param themeOverrides - The custom overrides for the base theme
 * @param themeAPI - The theme API schema that should be used for composition
 * @returns {{}} - The composed theme
 */

export const composeTheme = (
  theme: Object = {},
  themeOverrides: Object = {},
  themeAPI: Object = {}
) => {
  // Return theme if there are no overrides provided
  if (isEmpty(themeOverrides)) return theme;

  // final object to be returned
  const composedTheme = cloneDeep(themeAPI);

  for (const componentId in themeAPI) {
    if (hasProperty(composedTheme, componentId)) {
      const componentStyles = composedTheme[componentId];
      composeComponentStyles(componentStyles, theme[componentId]);
      composeComponentStyles(componentStyles, themeOverrides[componentId]);
    }
  }
  return composedTheme;
};

type ThemeProps = Object & {
  context: ThemeContextProp,
  themeId: string,
  theme: ?Object,
  themeOverrides: Object,
};

// Used in componentDidUpdate, this function compares the current
// set of theme related props against the next set to see if any have changed.
// If true, a component's theme is recomposed and local state is updated
export const didThemePropsChange = (
  { context, themeId, theme, themeOverrides }: ThemeProps,
  {
    context: nextContext,
    themeId: nextThemeId,
    theme: nextTheme,
    themeOverrides: nextOverrides,
  }: ThemeProps,
  setState: Function
) => {
  if (
    !isEqual(context, nextContext) ||
    !isEqual(themeId, nextThemeId) ||
    !isEqual(theme, nextTheme) ||
    !isEqual(themeOverrides, nextOverrides)
  ) {
    setState(() => ({
      composedTheme: composeTheme(
        addThemeId(nextTheme || nextContext.theme, nextThemeId),
        addThemeId(nextOverrides, nextThemeId),
        nextContext.ROOT_THEME_API
      ),
    }));
  }
};
