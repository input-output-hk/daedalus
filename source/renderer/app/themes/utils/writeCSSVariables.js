// @flow
import { has } from 'lodash';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';

const CARDANO_KEYS = Object.keys(CARDANO_THEME_CONFIG);
const DARK_BLUE_KEYS = Object.keys(DARK_BLUE_THEME_CONFIG);
const LIGHT_BLUE_KEYS = Object.keys(LIGHT_BLUE_THEME_CONFIG);

export const writeCSSVariables = (modifiedTheme: Object) => {
  // 1. check for new definitions on modifiedTheme
  // 2. check for new css variables added to existing definitions on modifiedTheme
  // 3. add new definitions and/or new css variables to modified theme
  const updatedCardanoTheme = addNewThemeDefinitions(
    CARDANO_THEME_CONFIG,
    modifiedTheme
  );
};

const findNewDefinitions = (
  existingTheme: Object,
  modifiedTheme: Object
): Array<?{}> => {
  const modifiedThemeKeys = Object.keys(modifiedTheme);
  const existingThemeKeys = Object.keys(existingTheme);

  const newDefinitions = modifiedThemeKeys
    .filter(key => existingThemeKeys.indexOf(key) < 0)
    .reduce(
      (accumulator, newKey) => [
        ...accumulator,
        { [newKey]: { ...modifiedTheme[newKey] } },
      ],
      []
    );

  // use filter instead.
  const newCSSVariables = modifiedThemeKeys.forEach(key => {
    if (has(existingTheme, key)) {
      const varsOnModifiedTheme = Object.keys(modifiedTheme[key]);
      const varsOnExistingTheme = Object.keys(existingTheme[key]);
      const newCSSVars = varsOnModifiedTheme.filter(
        cssVariable => varsOnExistingTheme.indexOf(cssVariable) < 0
      );
      if (newCSSVars.length) {
        // add new css variables to an object with existing key to accumulator array
      }
    }
  });

  return newDefinitions;
};

const findNewCSSVariables = (
  existingTheme: Object,
  modifiedTheme: Object
): Array<?{}> => {
  // flatten both existingTheme and modifiedTheme objects
  const allExistingVars = Object.values(existingTheme).reduce(
    (theme, componentVars) => ({ ...theme, ...componentVars }),
    {}
  );
  const themeWithNewVars = Object.values(modifiedTheme).reduce(
    (theme, componentVars) => ({ ...theme, ...componentVars }),
    {}
  );

  const newVars = modifiedThemeKeys.map(key => {
    if (existingThemeKeys.indexOf(key) < 0) {
      return Object.assign({}, { [key]: { ...modifiedTheme[key] } });
    }
  });

  return newVars;
};

const addNewThemeDefinitions = (
  existingTheme: Object,
  newDefinitions: Array<{}>
) => {
  return newDefinitions.reduce((updatedTheme, cssVarsToAdd) => {
    const keyName = Object.keys(cssVarsToAdd)[0];
    const newCSSVars = { ...cssVarsToAdd[keyName] };
    return updateTheme({ existingTheme: updatedTheme, keyName, newCSSVars });
  }, existingTheme);
};

const updateTheme = ({
  existingTheme,
  keyName,
  newCSSVars,
}: {
  existingTheme: Object,
  keyName: string,
  newCSSVars: Object,
}) => {
  if (has(existingTheme, keyName)) {
    return {
      ...existingTheme,
      [keyName]: {
        ...existingTheme[keyName],
        ...newCSSVars,
      },
    };
  }

  return {
    ...existingTheme,
    [keyName]: {
      ...newCSSVars,
    },
  };
};
