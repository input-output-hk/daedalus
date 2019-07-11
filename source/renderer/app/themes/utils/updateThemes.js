/* eslint-disable no-console */
// @flow
import { has, isEmpty } from 'lodash';
import chalk from 'chalk';
import { findMissingDefinitions, findMissingCSSVars } from './checkCreateTheme';
import { CARDANO_THEME_CONFIG } from '../daedalus/cardano';
import { DARK_BLUE_THEME_CONFIG } from '../daedalus/dark-blue';
import { LIGHT_BLUE_THEME_CONFIG } from '../daedalus/light-blue';
import type { DaedalusThemesUpdates } from '../types';

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
export const checkExistingThemes = (createThemeObj: Object): Object => {
  const daedalusThemesUpdates = {};
  const cardanoDefsToAdd = {
    ...findMissingDefinitions(createThemeObj, CARDANO_THEME_CONFIG),
    ...findMissingCSSVars(createThemeObj, CARDANO_THEME_CONFIG),
  };
  const darkBlueDefsToAdd = {
    ...findMissingDefinitions(createThemeObj, DARK_BLUE_THEME_CONFIG),
    ...findMissingCSSVars(createThemeObj, DARK_BLUE_THEME_CONFIG),
  };
  const lightBlueDefsToAdd = {
    ...findMissingDefinitions(createThemeObj, LIGHT_BLUE_THEME_CONFIG),
    ...findMissingCSSVars(createThemeObj, LIGHT_BLUE_THEME_CONFIG),
  };

  if (!isEmpty(cardanoDefsToAdd)) {
    logDifferences({
      color: '#2cbb69',
      missingDefs: cardanoDefsToAdd,
      themeName: 'cardano.js',
    });
    daedalusThemesUpdates.cardanoUpdates = cardanoDefsToAdd;
  }

  if (!isEmpty(darkBlueDefsToAdd)) {
    logDifferences({
      color: '#2874A6',
      missingDefs: darkBlueDefsToAdd,
      themeName: 'dark-blue.js',
    });
    daedalusThemesUpdates.darkBlueUpdates = darkBlueDefsToAdd;
  }

  if (!isEmpty(lightBlueDefsToAdd)) {
    logDifferences({
      color: '#33C4FF',
      missingDefs: lightBlueDefsToAdd,
      themeName: 'light-blue.js',
    });
    daedalusThemesUpdates.lightBlueUpdates = lightBlueDefsToAdd;
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
  }
  return daedalusThemesUpdates;
};

export const updateTheme = (existingTheme: Object, themeUpdates: Object) => {
  const updatedTheme = Object.entries(themeUpdates).reduce(
    (theme: Object, newEntry: [string, Object]) => {
      const [keyName, newCSSVars] = newEntry;
      if (keyName && has(theme, keyName)) {
        return {
          ...theme,
          [keyName]: {
            ...theme[keyName],
            ...newCSSVars,
          },
        };
      }
      if (keyName && !has(theme, keyName)) {
        return {
          ...theme,
          [keyName]: {
            ...newCSSVars,
          },
        };
      }
      return theme;
    },
    { ...existingTheme }
  );
  return updatedTheme;
};

export const updateThemes = (daedalusThemesUpdates: DaedalusThemesUpdates) => {
  const {
    cardanoUpdates,
    darkBlueUpdates,
    lightBlueUpdates,
  } = daedalusThemesUpdates;

  if (cardanoUpdates && !isEmpty(cardanoUpdates)) {
    const updatedCardanoTheme = updateTheme(
      CARDANO_THEME_CONFIG,
      cardanoUpdates
    );

    // write updatedCardanoTheme theme object to cardano.js
    // $FlowFixMe
    console.log(
      `cardano theme updated!\n${JSON.stringify(updatedCardanoTheme, 0, 2)}`
    );
  }

  if (darkBlueUpdates && !isEmpty(darkBlueUpdates)) {
    const updatedDarkBlueTheme = updateTheme(
      DARK_BLUE_THEME_CONFIG,
      darkBlueUpdates
    );

    // write updatedDarkBlueTheme theme object to dark-blue.js
    // $FlowFixMe
    console.log(
      `Dark Blue theme updated!\n${JSON.stringify(updatedDarkBlueTheme, 0, 2)}`
    );
  }

  if (lightBlueUpdates && !isEmpty(lightBlueUpdates)) {
    const updatedLightBlueTheme = updateTheme(
      LIGHT_BLUE_THEME_CONFIG,
      lightBlueUpdates
    );

    // write updatedLightBlueTheme theme object to light-blue.js
    // $FlowFixMe
    console.log(
      `Light Blue theme updated!\n${JSON.stringify(
        updatedLightBlueTheme,
        0,
        2
      )}`
    );
  }
};
