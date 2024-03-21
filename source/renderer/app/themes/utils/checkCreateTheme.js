'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.findMissingCSSVars = exports.findMissingDefinitions = exports.checkCreateTheme = void 0;
/* eslint-disable no-console */
const lodash_1 = require('lodash');
const chalk_1 = __importDefault(require('chalk'));
const index_1 = require('../daedalus/index');
const constants_1 = require('./constants');
const logDifferences = ({ color, missingDefs, themeName }) => {
  const message = (0,
  chalk_1.default)`\n{inverse  createTheme.js } is missing the following definitions that exist in the {underline ${themeName}} theme:\n\n${JSON.stringify(
    missingDefs,
    null,
    2
  )}\n`;
  return console.log(chalk_1.default.hex(color)(message));
};
// Checks for properties/CSS vars on existing themes that don't exist on createThemeObj
const checkCreateTheme = (createThemeObj) => {
  const missingDefinitions = index_1.EXISTING_THEME_OUTPUTS.reduce(
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    (defsToAdd, themeOutput) => {
      const [fileName, themeObj] = themeOutput;
      const missingDefs = {
        ...(0, exports.findMissingDefinitions)(themeObj, createThemeObj),
        ...(0, exports.findMissingCSSVars)(themeObj, createThemeObj),
      };
      if (!(0, lodash_1.isEmpty)(missingDefs)) {
        defsToAdd[fileName] = missingDefs;
      }
      return defsToAdd;
    },
    {}
  );
  // loop over missingDefinitions and log differences
  for (const themeName in missingDefinitions) {
    if (themeName && !(0, lodash_1.isEmpty)(missingDefinitions[themeName])) {
      logDifferences({
        color: constants_1.THEME_LOGGING_COLORS[themeName],
        // @ts-ignore ts-migrate(2322) FIXME: Type 'string | { aboutWindow: { '--theme-about-win... Remove this comment to see the full error message
        missingDefs: missingDefinitions[themeName],
        themeName,
      });
    }
  }
  if ((0, lodash_1.isEmpty)(missingDefinitions)) {
    console.log(
      chalk_1.default.hex('#2cbb69')(
        `\n${chalk_1.default.bold.inverse(
          '*** createTheme.js is up to date with all the Daedalus themes! ***'
        )}`
      )
    );
  }
};
exports.checkCreateTheme = checkCreateTheme;
const findMissingDefinitions = (basis, target) => {
  const targetMissingDefs = {};
  for (const basisEntry in basis) {
    if (basisEntry && !(0, lodash_1.has)(target, basisEntry)) {
      targetMissingDefs[basisEntry] = basis[basisEntry];
    }
  }
  return targetMissingDefs;
};
exports.findMissingDefinitions = findMissingDefinitions;
const findMissingCSSVars = (basis, target) => {
  const missingCSSVariables = {};
  for (const basisEntry in target) {
    if (basisEntry && (0, lodash_1.has)(target, basisEntry)) {
      for (const basisCSSVar in basis[basisEntry]) {
        if (
          basisCSSVar &&
          !(0, lodash_1.has)(target[basisEntry], basisCSSVar)
        ) {
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
exports.findMissingCSSVars = findMissingCSSVars;
//# sourceMappingURL=checkCreateTheme.js.map
