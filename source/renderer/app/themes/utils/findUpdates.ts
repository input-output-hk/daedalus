/* eslint-disable no-console */
import { isEmpty } from 'lodash';
import chalk from 'chalk';
import { EXISTING_THEME_OUTPUTS_OBJ } from '../daedalus/index';
import { THEME_LOGGING_COLORS } from './constants';
import { findMissingDefinitions, findMissingCSSVars } from './checkCreateTheme';
import type { PendingThemesUpdates, LogDifferencesParams } from '../types';

const logDifferences = ({
  color,
  missingDefs,
  themeName,
}: LogDifferencesParams) => {
  const message = chalk`\n{underline ${themeName}} is missing the following definitions that exist on the {bold createTheme} object:\n\n${JSON.stringify(
    missingDefs,
    null,
    2
  )}\n`;
  return console.log(chalk.hex(color)(message));
};

// Checks for properties/CSS vars on createThemeObj that don't exist on existing themes
export const findUpdates = (
  createThemeOutputs: Array<[string, Record<string, any>]>
): PendingThemesUpdates => {
  const pendingThemesUpdates = createThemeOutputs.reduce(
    (
      defsToAdd: Record<string, any>,
      themeOutput: [string, Record<string, any>]
    ) => {
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

  // loop over pendingThemesUpdates and log differences
  for (const themeName in pendingThemesUpdates) {
    if (themeName && !isEmpty(pendingThemesUpdates[themeName])) {
      logDifferences({
        color: THEME_LOGGING_COLORS[themeName],
        missingDefs: pendingThemesUpdates[themeName],
        themeName,
      });
    }
  }

  if (isEmpty(pendingThemesUpdates)) {
    console.log(
      chalk.hex('#2cbb69')(
        `\n${chalk.bold.inverse(
          '*** All Daedalus themes are up to date with the createTheme object! ***'
        )}`
      )
    );
  }

  return pendingThemesUpdates;
};
