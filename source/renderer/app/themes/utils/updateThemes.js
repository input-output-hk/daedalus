// @flow
import { has } from 'lodash';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';

const CARDANO_KEYS = Object.keys(CARDANO_THEME_CONFIG);
const DARK_BLUE_KEYS = Object.keys(DARK_BLUE_THEME_CONFIG);
const LIGHT_BLUE_KEYS = Object.keys(LIGHT_BLUE_THEME_CONFIG);

const findNewDefinitions = (
  existingTheme: Object,
  createThemeObj: Object
): Array<?{}> => {
  const createThemeObjKeys = Object.keys(createThemeObj);
  const existingThemeKeys = Object.keys(existingTheme);

  const newDefinitions = createThemeObjKeys
    .filter(key => existingThemeKeys.indexOf(key) < 0)
    .reduce(
      (accumulator, newKey) => [
        ...accumulator,
        { [newKey]: { ...createThemeObj[newKey] } },
      ],
      []
    );

  const newCSSVariables = createThemeObjKeys.filter(key => {
    if (has(existingTheme, key)) {
      const createThemeVariables = Object.keys(createThemeObj[key]);
      const existingThemeVariables = Object.keys(existingTheme[key]);
      const newCreateThemeVars = createThemeVariables.filter(
        cssVar => existingThemeVariables.indexOf(cssVar) < 0
      );
      if (newCreateThemeVars.length) {
        return newCreateThemeVars.reduce(
          (accumulator, newVar) => ({
            [key]: {
              ...accumulator[key],
              [newVar]: createThemeObj[key][newVar],
            },
          }),
          { [key]: {} }
        );
      }
    }
    return [];
  });

  return [...newDefinitions, ...newCSSVariables];
};

// const findNewCSSVariables = (
//   existingTheme: Object,
//   modifiedTheme: Object
// ): Array<?{}> => {
//   // flatten both existingTheme and modifiedTheme objects
//   const allExistingVars = Object.values(existingTheme).reduce(
//     (theme, componentVars) => ({ ...theme, ...componentVars }),
//     {}
//   );
//   const themeWithNewVars = Object.values(modifiedTheme).reduce(
//     (theme, componentVars) => ({ ...theme, ...componentVars }),
//     {}
//   );

//   const newVars = modifiedThemeKeys.map(key => {
//     if (existingThemeKeys.indexOf(key) < 0) {
//       return Object.assign({}, { [key]: { ...modifiedTheme[key] } });
//     }
//   });

//   return newVars;
// };

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

// Checks for properties/CSS vars on createThemeObj that don't exist on existing themes
export const checkExistingThemes = (createThemeObj: Object) => {
  const newCardanoDefs = findNewDefinitions(CARDANO_KEYS, createThemeObj);
  const newDarkBlueDefs = findNewDefinitions(DARK_BLUE_KEYS, createThemeObj);
  const newLightBlueDefs = findNewDefinitions(LIGHT_BLUE_KEYS, createThemeObj);

  newCardanoDefs.length &&
    console.log(`
    The Cardano theme is missing the following theme definitions that exist on the createTheme object:
    ${JSON.stringify(newCardanoDefs)}
   `);

  newDarkBlueDefs.length &&
    console.log(`
    The Dark-Blue theme is missing the following theme definitions that exist on the createTheme object:
    ${JSON.stringify(newDarkBlueDefs)}
   `);

  newLightBlueDefs.length &&
    console.log(`
    The Light-Blue theme is missing the following theme definitions that exist on the createTheme object:
    ${JSON.stringify(newLightBlueDefs)}
   `);
};

export const updateThemes = (createThemeObj: Object, existingTheme: Object) => {
  const updatedCardanoTheme = addNewThemeDefinitions(
    existingTheme,
    createThemeObj
  );
};
