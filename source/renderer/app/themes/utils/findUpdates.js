'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.findUpdates = void 0;
/* eslint-disable no-console */
const lodash_1 = require('lodash');
const chalk_1 = __importDefault(require('chalk'));
const index_1 = require('../daedalus/index');
const constants_1 = require('./constants');
const checkCreateTheme_1 = require('./checkCreateTheme');
const logDifferences = ({ color, missingDefs, themeName }) => {
  const message = (0,
  chalk_1.default)`\n{underline ${themeName}} is missing the following definitions that exist on the {bold createTheme} object:\n\n${JSON.stringify(
    missingDefs,
    null,
    2
  )}\n`;
  return console.log(chalk_1.default.hex(color)(message));
};
// Checks for properties/CSS vars on createThemeObj that don't exist on existing themes
const findUpdates = (createThemeOutputs) => {
  const pendingThemesUpdates = createThemeOutputs.reduce(
    (defsToAdd, themeOutput) => {
      const [fileName, themeObj] = themeOutput;
      const missingDefs = {
        ...(0, checkCreateTheme_1.findMissingDefinitions)(
          themeObj,
          index_1.EXISTING_THEME_OUTPUTS_OBJ[fileName]
        ),
        ...(0, checkCreateTheme_1.findMissingCSSVars)(
          themeObj,
          index_1.EXISTING_THEME_OUTPUTS_OBJ[fileName]
        ),
      };
      if (!(0, lodash_1.isEmpty)(missingDefs)) {
        defsToAdd[fileName] = missingDefs;
      }
      return defsToAdd;
    },
    {}
  );
  // loop over pendingThemesUpdates and log differences
  for (const themeName in pendingThemesUpdates) {
    if (themeName && !(0, lodash_1.isEmpty)(pendingThemesUpdates[themeName])) {
      logDifferences({
        color: constants_1.THEME_LOGGING_COLORS[themeName],
        missingDefs: pendingThemesUpdates[themeName],
        themeName,
      });
    }
  }
  if ((0, lodash_1.isEmpty)(pendingThemesUpdates)) {
    console.log(
      chalk_1.default.hex('#2cbb69')(
        `\n${chalk_1.default.bold.inverse(
          '*** All Daedalus themes are up to date with the createTheme object! ***'
        )}`
      )
    );
  }
  return pendingThemesUpdates;
};
exports.findUpdates = findUpdates;
//# sourceMappingURL=findUpdates.js.map
