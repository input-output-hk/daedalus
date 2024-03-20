// @ts-nocheck
import React, { useMemo, useState, useEffect } from 'react';

// external libraries
import { isEmpty, cloneDeep } from 'lodash';

// contains default theme and context provider
import { ThemeContext } from './HOC/ThemeContext';

// imports the Root Theme API object which specifies the shape
// of a complete theme for every component in this library, used in this.composeLibraryTheme
import { ROOT_THEME_API } from '../themes/API';

// internal utility functions
import { appendToProperty } from '../utils/themes';
import { hasProperty } from '../utils/props';
import { ThemeVariablesProvider } from './ThemeVariablesProvider';

function ThemeProvider({
  children,
  skins,
  theme,
  variables,
  isRoot,
  themeOverrides,
}) {
  const [themeState, setThemeState] = useState(() =>
    composeLibraryTheme(theme, themeOverrides)
  );

  useEffect(() => {
    setThemeState(composeLibraryTheme(theme, themeOverrides));
  }, [theme, themeOverrides]);

  const providerValue = useMemo(
    () => ({
      skins,
      theme: themeState,
      ROOT_THEME_API,
    }),
    [skins, themeState]
  );

  return (
    <ThemeContext.Provider value={providerValue}>
      <ThemeVariablesProvider isRoot={isRoot} variables={variables}>
        {children}
      </ThemeVariablesProvider>
    </ThemeContext.Provider>
  );
}

ThemeProvider.defaultProps = {
  isRoot: true,
  skins: {},
  theme: {},
  variables: {},
  themeOverrides: {},
};

const composeLibraryTheme = (theme, themeOverrides) => {
  if (isEmpty(themeOverrides)) {
    return theme;
  }

  const composedTheme = {};

  Object.keys(ROOT_THEME_API).forEach((componentName) => {
    if (hasProperty(theme, componentName)) {
      composedTheme[componentName] = theme[componentName];
    }

    if (hasProperty(themeOverrides, componentName)) {
      composedTheme[componentName] = applyThemeOverrides(
        theme[componentName],
        themeOverrides[componentName],
        ROOT_THEME_API[componentName]
      );
    }
  });

  return composedTheme;
};

const applyThemeOverrides = (
  componentTheme,
  componentThemeOverrides,
  componentThemeAPI
) => {
  if (isEmpty(componentThemeOverrides)) {
    return componentTheme;
  }

  const composedComponentTheme = cloneDeep(componentThemeAPI);

  Object.keys(componentThemeAPI).forEach((className) => {
    if (hasProperty(componentTheme, className)) {
      appendToProperty(
        composedComponentTheme,
        className,
        componentTheme[className]
      );
    }

    if (hasProperty(componentThemeOverrides, className)) {
      appendToProperty(
        composedComponentTheme,
        className,
        componentThemeOverrides[className]
      );
    }
  });

  return composedComponentTheme;
};

export { ThemeProvider };
