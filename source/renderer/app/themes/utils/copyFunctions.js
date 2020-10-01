// @flow
// import { isEmpty } from 'lodash';
// import readline from 'readline';
import chalk from 'chalk';
import inquirer from 'inquirer';
// import commander from 'commander';

import { EXISTING_THEME_OUTPUTS } from '../daedalus/index';

// const [, , action: Action, copyFrom: string, copyTo: string] = process.argv;

// if (action === 'category') {

// }

// console.log('copyFrom', copyFrom);
// console.log('copyTo', copyTo);

// EXISTING_THEME_OUTPUTS.forEach(([,theme]) => {
//   console.log('theme', theme);

// })

const copy = async () => {
  const actionChoices = [
    {
      value: 'property',
      short: 'property',
      name: chalk.reset(
        `• Copy a single ${chalk.blue.bold(
          'PROPERTY'
        )} (e.g. "--theme-topbar-layout-body-background-color")`
      ),
    },
    {
      value: 'properties',
      short: 'properties',
      name: chalk.reset(
        `• Copy all ${chalk.blue.bold(
          'PROPERTIES'
        )} that start with... (e.g. "--theme-transactions-list-{...}")`
      ),
    },
    {
      value: 'category',
      short: 'category',
      name: chalk.reset(
        `• Copy an entire ${chalk.blue.bold(
          'CATEGORY'
        )} (e.g. "aboutWindow", "newsFeed", "topBar")`
      ),
    },
  ];

  const { action } = await inquirer.prompt([
    {
      name: 'action',
      type: 'list',
      message: 'What do you want to copy?',
      choices: actionChoices,
    },
  ]);

  console.log('action', action);
  // .then(answers => {
  //   console.log('answers', answers);
  //   // Use user feedback for... whatever!!
  // })
  // .catch(error => {
  //   console.log('error', error);
  //   if (error.isTtyError) {
  //     console.log('isTtyError');
  //     // Prompt couldn't be rendered in the current environment
  //   } else {
  //     console.log('sth else');
  //     // Something else when wrong
  //   }
  // });
};

copy();
