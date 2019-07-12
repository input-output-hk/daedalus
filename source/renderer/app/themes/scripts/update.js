// @flow
import chalk from 'chalk';
import { findUpdates, updateThemes } from '../utils/updateThemes';
import { CREATE_THEME_OBJ } from '../utils/constants';

const logInputOption = (option: string) => chalk.magenta.italic(`'${option}'`);
const YES_OR_NO = `${logInputOption('yes')} / ${logInputOption('no')}`;

const readline = require('readline');

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: chalk.cyan(
    `\nWould you like to write these updates to the theme files? ( ${YES_OR_NO} )\n`
  ),
});

const pendingUpdates = findUpdates(CREATE_THEME_OBJ); // Todo: Enter correct params
if (pendingUpdates !== null) {
  rl.prompt();
  rl.on('line', line => {
    switch (line.trim()) {
      case 'yes' || 'y':
        chalk`\n{bold Upating themes...}\n`;
        const updatedThemes = updateThemes(pendingUpdates);
        for (const updatedTheme of Object.entries(updatedThemes)) {
          const [file, themeObj] = updatedTheme;
          // call fs module to write updates
          //write the themeObj to the file using the fs module
        }
        chalk
          .hex('#2cbb69')
          .bold(
            `\nThemes updated! Please check the result and commit the updates if neccessary.\n`
          );
        break;
      case 'no' || 'n':
        chalk.cyan('Exiting... See you later!');
        process.exit(0);
        break;
      default:
        chalk.cyan(
          `Umm... Enter ( ${YES_OR_NO} ) to continue, or ${logInputOption(
            'ctrl + c'
          )} to exit.`
        );
        break;
    }
    rl.prompt();
  }).on('close', () => {
    chalk.cyan('Exiting... See you later!');
    process.exit(0);
  });
}
