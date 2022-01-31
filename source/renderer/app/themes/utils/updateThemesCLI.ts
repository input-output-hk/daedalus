import { isEmpty } from 'lodash';
import readline from 'readline';
import chalk from 'chalk';
import { updateThemes } from './updateThemes';
import { writeThemeUpdate } from './writeThemeUpdate';
import type { PendingThemesUpdates } from '../types';

/* eslint-disable-next-line no-console */
const logMsg = (msg: string) => console.log(msg);

const logInputOption = (option: string) => chalk.magenta.italic(`'${option}'`);

const YES_OR_NO = `${logInputOption('yes')} / ${logInputOption('no')}`;
export const runUpdateThemesCLI = (pendingUpdates: PendingThemesUpdates) => {
  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: chalk`\n{cyan Would you like to write these updates to the theme files?} ( ${YES_OR_NO} )\n`,
  });
  rl.prompt();
  rl.on('line', (line) => {
    switch (line.trim()) {
      case 'yes': {
        logMsg(chalk`\n{cyan Upating themes...}\n`);
        // combines pending theme updates with existing theme outputs
        const updatedThemes = updateThemes(pendingUpdates);

        for (const themeName in updatedThemes) {
          if (themeName && !isEmpty(updatedThemes[themeName])) {
            const fileName = themeName.split('.')[0];
            const updatedThemeObj = updatedThemes[themeName];
            writeThemeUpdate({
              fileName,
              updatedThemeObj,
            });
          }
        }

        logMsg(chalk`\n{greenBright Themes are up to date!}\n`);
        logMsg(
          chalk`{cyan Prettier is now formatting the updated theme files.}`
        );
        logMsg(
          chalk`{cyan Once Prettier finshes, please check the diff and commit the changes if all is correct.}`
        );
        logMsg(chalk`\n{cyan Prettier in progress...}\n`);
        process.exit(0);
        break;
      }

      case 'no': {
        logMsg(chalk`\n{cyan Exiting... See you later!}\n`);
        process.exit(0);
        break;
      }

      default: {
        logMsg(
          chalk.cyan(
            `\nCommand not recognized.\n\nEnter ( ${YES_OR_NO} ) to continue, or ${logInputOption(
              'ctrl + c'
            )} to exit.`
          )
        );
        break;
      }
    }
  }).on('close', () => {
    logMsg(chalk`\n{cyan Exiting... See you later!}\n`);
    process.exit(0);
  });
};
