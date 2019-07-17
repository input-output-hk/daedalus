// @flow
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
    prompt: chalk.cyan(
      `\nWould you like to write these updates to the theme files? ( ${YES_OR_NO} )\n`
    ),
  });

  rl.prompt();

  rl.on('line', line => {
    switch (line.trim()) {
      case 'yes': {
        /* eslint-disable-next-line no-unused-expressions */
        chalk`\n{bold Upating themes...}\n`;
        // returns updated theme objects in 'daedalus/themes'
        const updatedThemes = updateThemes(pendingUpdates);

        for (const updatedTheme of Object.entries(updatedThemes)) {
          const [fileName, updatedThemeObj] = updatedTheme;
          writeThemeUpdate({ fileName, updatedThemeObj });
        }

        logMsg(
          chalk
            .hex('#2cbb69')
            .bold(
              `\nThemes are up to date! Prettier is now formatting the updated theme files. Please check the diff and commit the changes if all is correct.\n`
            )
        );
        process.exit(0);
        break;
      }

      case 'no': {
        logMsg(chalk.cyan('Exiting... See you later!'));
        process.exit(0);
        break;
      }

      default: {
        logMsg(
          chalk.cyan(
            `\nCommand not recognized.\nEnter ( ${YES_OR_NO} ) to continue, or ${logInputOption(
              'ctrl + c'
            )} to exit.`
          )
        );
        break;
      }
    }
  }).on('close', () => {
    logMsg(chalk.cyan('Exiting... See you later!'));
    process.exit(0);
  });
};
