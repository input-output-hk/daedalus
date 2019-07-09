// @flow
import { has, isEmpty } from 'lodash';
import { createTheme } from './createTheme';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';

// Checks for properties/CSS vars on existing themes that don't exist on createThemeObj
export const checkCreateTheme = (createThemeObj: Object) => {
  const missingCardanoDefs = {
    ...findMissingDefinitions(CARDANO_THEME_CONFIG, createThemeObj),
    ...findMissingCSSVars(CARDANO_THEME_CONFIG, createThemeObj),
  };
  const missingDarkBlueDefs = {
    ...findMissingDefinitions(DARK_BLUE_THEME_CONFIG, createThemeObj),
    ...findMissingCSSVars(DARK_BLUE_THEME_CONFIG, createThemeObj),
  };
  const missingLightBlueDefs = {
    ...findMissingDefinitions(LIGHT_BLUE_THEME_CONFIG, createThemeObj),
    ...findMissingCSSVars(LIGHT_BLUE_THEME_CONFIG, createThemeObj),
  };

  !isEmpty(missingCardanoDefs) &&
    console.log(`
      createTheme is missing the following theme definitions that exist in the Cardano theme:
      ${JSON.stringify(missingCardanoDefs, 0, 2)}
    `);

  !isEmpty(missingDarkBlueDefs) &&
    console.log(`
      createTheme is missing the following theme definitions that exist in the Dark-Blue theme:
      ${JSON.stringify(missingDarkBlueDefs, 0, 2)}
    `);

  !isEmpty(missingLightBlueDefs) &&
    console.log(`
      createTheme is missing the following theme definitions that exist in the Light-Blue theme:
      ${JSON.stringify(missingLightBlueDefs, 0, 2)}
    `);
};

export const findMissingDefinitions = (
  basis: Object,
  target: Object
): Object => {
  const targetMissingDefs = {};

  for (const basisEntry in basis) {
    if (basisEntry && !has(target, basisEntry)) {
      targetMissingDefs[basisEntry] = basis[basisEntry];
    }
  }
  return targetMissingDefs;
};

export const findMissingCSSVars = (basis: Object, target: Object): Object => {
  const missingCSSVariables = {};

  for (const basisEntry in target) {
    if (basisEntry && has(target, basisEntry)) {
      for (const basisCSSVar in basis[basisEntry]) {
        if (basisCSSVar && !has(target[basisEntry], basisCSSVar)) {
          missingCSSVariables[basisEntry] = {
            ...missingCSSVariables[basisEntry],
            [basisCSSVar]: basis[basisEntry][basisCSSVar],
          };
        }
      }
    }
  }
  return missingCSSVariables;
};

console.log(JSON.stringify(createTheme({ config: CARDANO_THEME_CONFIG })));
