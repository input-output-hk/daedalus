'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.runUpdateThemesCLI = void 0;
const lodash_1 = require('lodash');
const readline_1 = __importDefault(require('readline'));
const chalk_1 = __importDefault(require('chalk'));
const updateThemes_1 = require('./updateThemes');
const writeThemeUpdate_1 = require('./writeThemeUpdate');
/* eslint-disable-next-line no-console */
const logMsg = (msg) => console.log(msg);
const logInputOption = (option) =>
  chalk_1.default.magenta.italic(`'${option}'`);
const YES_OR_NO = `${logInputOption('yes')} / ${logInputOption('no')}`;
const runUpdateThemesCLI = (pendingUpdates) => {
  const rl = readline_1.default.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: (0,
    chalk_1.default)`\n{cyan Would you like to write these updates to the theme files?} ( ${YES_OR_NO} )\n`,
  });
  rl.prompt();
  rl.on('line', (line) => {
    switch (line.trim()) {
      case 'yes': {
        logMsg((0, chalk_1.default)`\n{cyan Updating themes...}\n`);
        // combines pending theme updates with existing theme outputs
        const updatedThemes = (0, updateThemes_1.updateThemes)(pendingUpdates);
        for (const themeName in updatedThemes) {
          if (themeName && !(0, lodash_1.isEmpty)(updatedThemes[themeName])) {
            const fileName = themeName.split('.')[0];
            const updatedThemeObj = updatedThemes[themeName];
            (0, writeThemeUpdate_1.writeThemeUpdate)({
              fileName,
              updatedThemeObj,
            });
          }
        }
        logMsg((0, chalk_1.default)`\n{greenBright Themes are up to date!}\n`);
        logMsg(
          (0,
          chalk_1.default)`{cyan Prettier is now formatting the updated theme files.}`
        );
        logMsg(
          (0,
          chalk_1.default)`{cyan Once Prettier finishes, please check the diff and commit the changes if all is correct.}`
        );
        logMsg((0, chalk_1.default)`\n{cyan Prettier in progress...}\n`);
        process.exit(0);
        break;
      }
      case 'no': {
        logMsg((0, chalk_1.default)`\n{cyan Exiting... See you later!}\n`);
        process.exit(0);
        break;
      }
      default: {
        logMsg(
          chalk_1.default.cyan(
            `\nCommand not recognized.\n\nEnter ( ${YES_OR_NO} ) to continue, or ${logInputOption(
              'ctrl + c'
            )} to exit.`
          )
        );
        break;
      }
    }
  }).on('close', () => {
    logMsg((0, chalk_1.default)`\n{cyan Exiting... See you later!}\n`);
    process.exit(0);
  });
};
exports.runUpdateThemesCLI = runUpdateThemesCLI;
//# sourceMappingURL=updateThemesCLI.js.map
