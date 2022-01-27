/* eslint-disable no-console */
import { has, isEmpty } from 'lodash';
import chalk from 'chalk';
import { EXISTING_THEME_OUTPUTS } from '../daedalus/index';
import { THEME_LOGGING_COLORS } from './constants';
import type { LogDifferencesParams } from '../types';

const logDifferences = ({
  color,
  missingDefs,
  themeName,
}: LogDifferencesParams) => {
  const message = chalk`\n{inverse  createTheme.js } is missing the following definitions that exist in the {underline ${themeName}} theme:\n\n${JSON.stringify(
    missingDefs,
    null,
    2
  )}\n`;
  return console.log(chalk.hex(color)(message));
};

// Checks for properties/CSS vars on existing themes that don't exist on createThemeObj
export const checkCreateTheme = (createThemeObj: Record<string, any>) => {
  const missingDefinitions = EXISTING_THEME_OUTPUTS.reduce(
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    (
      defsToAdd: Record<string, any>,
      themeOutput: [string, Record<string, any>]
    ) => {
      const [fileName, themeObj] = themeOutput;
      const missingDefs = {
        ...findMissingDefinitions(themeObj, createThemeObj),
        ...findMissingCSSVars(themeObj, createThemeObj),
      };

      if (!isEmpty(missingDefs)) {
        defsToAdd[fileName] = missingDefs;
      }

      return defsToAdd;
    },
    {}
  );

  // loop over missingDefinitions and log differences
  for (const themeName in missingDefinitions) {
    if (themeName && !isEmpty(missingDefinitions[themeName])) {
      logDifferences({
        color: THEME_LOGGING_COLORS[themeName],
        // @ts-ignore ts-migrate(2322) FIXME: Type 'string | { aboutWindow: { '--theme-about-win... Remove this comment to see the full error message
        missingDefs: missingDefinitions[themeName],
        themeName,
      });
    }
  }

  if (isEmpty(missingDefinitions)) {
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
  basis: Record<string, any>,
  target: Record<string, any>
): Record<string, any> => {
  const targetMissingDefs = {};

  for (const basisEntry in basis) {
    if (basisEntry && !has(target, basisEntry)) {
      targetMissingDefs[basisEntry] = basis[basisEntry];
    }
  }

  return targetMissingDefs;
};
export const findMissingCSSVars = (
  basis: Record<string, any>,
  target: Record<string, any>
): Record<string, any> => {
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
