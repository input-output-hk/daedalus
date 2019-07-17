/* eslint-disable no-console */
// @flow
import { isEmpty } from 'lodash';
import chalk from 'chalk';
import { findMissingDefinitions, findMissingCSSVars } from './checkCreateTheme';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';
import type {
  PendingThemesUpdates,
  FindUpdatesParams,
  LogDifferencesParams,
} from '../types';

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

// Checks for properties/CSS vars on createThemeObj that don't exist on existing themes
export const findUpdates = (
  createThemeOutputs: FindUpdatesParams
): null | PendingThemesUpdates => {
  const { cardano, darkBlue, lightBlue } = createThemeOutputs;
  const pendingThemesUpdates = {};

  const cardanoDefsToAdd = {
    ...findMissingDefinitions(cardano, CARDANO_THEME_CONFIG),
    ...findMissingCSSVars(cardano, CARDANO_THEME_CONFIG),
  };
  const darkBlueDefsToAdd = {
    ...findMissingDefinitions(darkBlue, DARK_BLUE_THEME_CONFIG),
    ...findMissingCSSVars(darkBlue, DARK_BLUE_THEME_CONFIG),
  };
  const lightBlueDefsToAdd = {
    ...findMissingDefinitions(lightBlue, LIGHT_BLUE_THEME_CONFIG),
    ...findMissingCSSVars(lightBlue, LIGHT_BLUE_THEME_CONFIG),
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

  if (!isEmpty(lightBlueDefsToAdd)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: lightBlueDefsToAdd,
      themeName: 'light-blue.js',
    });
    pendingThemesUpdates.lightBlueUpdates = lightBlueDefsToAdd;
  }

  if (
    isEmpty(cardanoDefsToAdd) &&
    isEmpty(darkBlueDefsToAdd) &&
    isEmpty(lightBlueDefsToAdd)
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
