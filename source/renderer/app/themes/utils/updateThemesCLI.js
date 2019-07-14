// @flow
/* eslint-disable */
import readline from 'readline';
import chalk from 'chalk';
import { updateThemes } from './updateThemes';
import { writeThemeUpdate } from './writeThemeUpdate';
import type { PendingThemesUpdates } from '../types';

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
      case 'yes' || 'y': {
        chalk`\n{bold Upating themes...}\n`;
        const updatedThemes = updateThemes(pendingUpdates);

        for (const updatedTheme of Object.entries(updatedThemes)) {
          const [file, updatedThemeObj] = updatedTheme;
          const fileName = `${file}.js`;
          // should this by async?
          writeThemeUpdate({ fileName, updatedThemeObj });
        }

        chalk
          .hex('#2cbb69')
          .bold(
            `\nThemes updated! Please check the result and commit the updates if neccessary.\n`
          );
        break;
      }

      case 'no' || 'n': {
        chalk.cyan('Exiting... See you later!');
        process.exit(0);
        break;
      }

      default: {
        chalk.cyan(
          `Umm... Enter ( ${YES_OR_NO} ) to continue, or ${logInputOption(
            'ctrl + c'
          )} to exit.`
        );
        break;
      }
    }
    rl.prompt();
  }).on('close', () => {
    chalk.cyan('Exiting... See you later!');
    process.exit(0);
  });
};
