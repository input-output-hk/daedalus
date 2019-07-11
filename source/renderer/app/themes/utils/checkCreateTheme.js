/* eslint-disable no-unused-expressions */
/* eslint-disable no-console */
// @flow
import { has, isEmpty } from 'lodash';
import chalk from 'chalk';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';
import type { LogDifferencesParams } from '../types';

const logDifferences = ({
  color,
  missingDefs,
  themeName,
}: LogDifferencesParams) => {
  // $FlowFixMe
  const message = chalk`\n{inverse  createTheme.js } is missing the following definitions that exist in the {underline ${themeName}} theme:\n\n${JSON.stringify(
    missingDefs,
    0,
    2
  )}\n`;
  return console.log(chalk.hex(color)(message));
};

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

  if (!isEmpty(missingCardanoDefs)) {
    logDifferences({
      color: '#2cbb69',
      missingDefs: missingCardanoDefs,
      themeName: 'cardano.js',
    });
  }

  if (!isEmpty(missingDarkBlueDefs)) {
    logDifferences({
      color: '#2874A6',
      missingDefs: missingDarkBlueDefs,
      themeName: 'dark-blue.js',
    });
  }

  if (!isEmpty(missingLightBlueDefs)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: missingLightBlueDefs,
      themeName: 'light-blue.js',
    });
  }

  if (
    isEmpty(missingCardanoDefs) &&
    isEmpty(missingDarkBlueDefs) &&
    isEmpty(missingLightBlueDefs)
  ) {
    console.log(
      chalk.hex('#2cbb69')(
        `\n${chalk.bold.inverse(
          '*** createTheme.js is up to date with all the Daedalus themes! ***'
        )}`
      )
    );
  }
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
