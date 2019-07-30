/* eslint-disable no-console */
// @flow
import { isEmpty } from 'lodash';
import chalk from 'chalk';
import { EXISTING_THEME_OUTPUTS } from '../daedalus/index.js';
import { findMissingDefinitions, findMissingCSSVars } from './checkCreateTheme';
import type { PendingThemesUpdates, LogDifferencesParams } from '../types';

const logDifferences = ({
  color,
  missingDefs,
  themeName,
}: LogDifferencesParams) => {
  // $FlowFixMe
  const message = chalk`\n{underline ${themeName}} is missing the following definitions that exist on the {bold createTheme} object:\n\n${JSON.stringify(
    missingDefs,
    0,
    2
  )}\n`;
  return console.log(chalk.hex(color)(message));
};

const EXISTING_THEME_OUTPUTS_OBJ = Object.fromEntries(EXISTING_THEME_OUTPUTS);

// Checks for properties/CSS vars on createThemeObj that don't exist on existing themes
export const findUpdates = (
  createThemeOutputs: Array<[string, Object]>
): null | PendingThemesUpdates => {
  const pendingThemesUpdates = createThemeOutputs.reduce(
    (defsToAdd, themeOutput) => {
      const [fileName, themeObj] = themeOutput;

      const missingDefs = {
        ...findMissingDefinitions(
          themeObj,
          EXISTING_THEME_OUTPUTS_OBJ[fileName]
        ),
        ...findMissingCSSVars(themeObj, EXISTING_THEME_OUTPUTS_OBJ[fileName]),
      };
      if (!isEmpty(missingDefs)) {
        defsToAdd[fileName] = missingDefs;
      }
      return defsToAdd;
    },
    {}
  );

  // TODO: finish refactoring below
  const cardanoDefsToAdd = {
    ...findMissingDefinitions(cardano, CARDANO_THEME_CONFIG),
    ...findMissingCSSVars(cardano, CARDANO_THEME_CONFIG),
  };
  const darkBlueDefsToAdd = {
    ...findMissingDefinitions(darkBlue, DARK_BLUE_THEME_CONFIG),
    ...findMissingCSSVars(darkBlue, DARK_BLUE_THEME_CONFIG),
  };
  const darkCardanoDefsToAdd = {
    ...findMissingDefinitions(darkCardano, DARK_CARDANO_THEME_CONFIG),
    ...findMissingCSSVars(darkCardano, DARK_CARDANO_THEME_CONFIG),
  };
  const lightBlueDefsToAdd = {
    ...findMissingDefinitions(lightBlue, LIGHT_BLUE_THEME_CONFIG),
    ...findMissingCSSVars(lightBlue, LIGHT_BLUE_THEME_CONFIG),
  };
  const yellowDefsToAdd = {
    ...findMissingDefinitions(yellow, YELLOW_THEME_CONFIG),
    ...findMissingCSSVars(yellow, YELLOW_THEME_CONFIG),
  };
  const whiteDefsToAdd = {
    ...findMissingDefinitions(white, WHITE_THEME_CONFIG),
    ...findMissingCSSVars(white, WHITE_THEME_CONFIG),
  };

  if (!isEmpty(cardanoDefsToAdd)) {
    logDifferences({
      color: '#2cbb69',
      missingDefs: cardanoDefsToAdd,
      themeName: 'cardano.js',
    });
    pendingThemesUpdates.cardanoUpdates = cardanoDefsToAdd;
  }

  if (!isEmpty(darkBlueDefsToAdd)) {
    logDifferences({
      color: '#2874A6',
      missingDefs: darkBlueDefsToAdd,
      themeName: 'dark-blue.js',
    });
    pendingThemesUpdates.darkBlueUpdates = darkBlueDefsToAdd;
  }

  if (!isEmpty(darkCardanoDefsToAdd)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: darkCardanoDefsToAdd,
      themeName: 'dark-cardano.js',
    });
    pendingThemesUpdates.darkCardanoUpdates = darkCardanoDefsToAdd;
  }

  if (!isEmpty(lightBlueDefsToAdd)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: lightBlueDefsToAdd,
      themeName: 'light-blue.js',
    });
    pendingThemesUpdates.lightBlueUpdates = lightBlueDefsToAdd;
  }

  if (!isEmpty(yellowDefsToAdd)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: yellowDefsToAdd,
      themeName: 'yellow.js',
    });
    pendingThemesUpdates.yellowUpdates = yellowDefsToAdd;
  }

  if (!isEmpty(whiteDefsToAdd)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: whiteDefsToAdd,
      themeName: 'white.js',
    });
    pendingThemesUpdates.whiteUpdates = whiteDefsToAdd;
  }

  if (
    isEmpty(cardanoDefsToAdd) &&
    isEmpty(darkBlueDefsToAdd) &&
    isEmpty(darkCardanoDefsToAdd) &&
    isEmpty(lightBlueDefsToAdd) &&
    isEmpty(yellowDefsToAdd) &&
    isEmpty(whiteDefsToAdd)
  ) {
    console.log(
      chalk.hex('#2cbb69')(
        `\n${chalk.bold.inverse(
          '*** All Daedalus themes are up to date with the createTheme object! ***'
        )}`
      )
    );
    return null;
  }
  return pendingThemesUpdates;
};
