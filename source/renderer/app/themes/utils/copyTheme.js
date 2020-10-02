// @flow
import chalk from 'chalk';
import inquirer from 'inquirer';
import { EXISTING_THEME_OUTPUTS } from '../daedalus/index';

// Utils
const { log } = console;
const { cyan, red, magenta } = chalk;
const orange = content => chalk.keyword('orange')(content);
const separator = () => log('\n');
const info = message => log(orange(message));
const warn = message => log(red(message));

// Config
const MAX_RESULTS_BEFORE_WARNING = 30;
const firstTheme = EXISTING_THEME_OUTPUTS[0][1];
const flatProperties = Object.values(firstTheme).reduce(
  (properties, category) => {
    return [...Object.entries(category), ...properties];
  },
  []
);

// Types
type Property = Array<string>;

const getChoicesFromProperties = properties =>
  properties.map(([name, value]) => ({
    value: [name, value],
    short: `\n✔ ${name}`,
    name: `${cyan(name)}: ${magenta(value)}`,
    checked: true,
  }));

const replaceSingleProperty = (
  property: Property,
  fromPrefix: string,
  toPrefix
) => {
  const [key, value] = property;
  const fromPrefixNoDash = removeLastDash(fromPrefix);
  const toPrefixNoDash = removeLastDash(toPrefix);
  const newPropertyKey = key.replace(fromPrefixNoDash, toPrefixNoDash);
  return [newPropertyKey, value];
};

const removeLastDash = property => {
  let prop = property;
  if (property.slice(-1) === '-') prop = prop.replace(/-$/, '');
  return prop;
};

const copy = async () => {
  /**
   * STEP 1
   * Get the properties prefix.
   * E.g. "--theme-wallet-import-..."
   */

  separator();

  const { fromPrefix } = await inquirer.prompt([
    {
      name: 'fromPrefix',
      type: 'input',
      message: `What is the property prefix?\ne.g. ${cyan(
        '--theme-wallet-import-'
      )}\n--->`,
    },
  ]);

  if (!fromPrefix) throw new Error('Invalid prefix');

  const properties = flatProperties.filter(
    ([property]) => property.indexOf(fromPrefix) > -1
  );

  // No properties found
  if (!properties.length) {
    warn(
      `I couldn't find any property with the prefix "${cyan(
        fromPrefix
      )}". Please, try again.`
    );
    return copy();
  }

  // Too many properties found (> MAX_RESULTS_BEFORE_WARNIN)
  if (properties.length > MAX_RESULTS_BEFORE_WARNING) {
    const { shouldProceed } = await inquirer.prompt([
      {
        name: 'shouldProceed',
        type: 'confirm',
        message: red(
          `I've found over ${properties.length} properties. Are you sure you want to proceed?`
        ),
      },
    ]);
    if (!shouldProceed) {
      info(`Ok, restarting...`);
      return copy();
    }
  }

  separator();

  /**
   * STEP 2
   * Select properties to be copied over
   */

  const { selectedProperties } = await inquirer.prompt([
    {
      name: 'selectedProperties',
      type: 'checkbox',
      message: chalk`Great! I've found ${properties.length} properties. Which ones should I copy over?`,
      choices: getChoicesFromProperties(properties),
      loop: false,
    },
  ]);

  if (!selectedProperties.length) {
    warn('Sorry, no properties were selected. Restarting...');
    return copy();
  }

  separator();

  /**
   * STEP 3
   * New prefix to copy into
   */

  const { toPrefix } = await inquirer.prompt([
    {
      name: 'toPrefix',
      type: 'input',
      message: `What is the new property prefix I should copy into?\ne.g. ${cyan(
        '--theme-new-super-feature-'
      )}\n--->`,
    },
  ]);

  if (!toPrefix) throw new Error('Invalid prefix');

  const [firstProperty] = properties;
  const [newKey, newValue] = replaceSingleProperty(
    firstProperty,
    fromPrefix,
    toPrefix
  );

  separator();

  const confirmMessage = `Great, I'll copy ${
    selectedProperties.length
  } properties from "${cyan(fromPrefix)}" to "${cyan(toPrefix)}".
Here's an example of how they will look like:
{ ${cyan(newKey)}: ${magenta(newValue)} }
Should I proceed?
`;

  const { confirmCopy } = await inquirer.prompt([
    {
      name: 'confirmCopy',
      type: 'confirm',
      message: confirmMessage,
    },
  ]);
  if (!confirmCopy) {
    info(`Ok, restarting...`);
    return copy();
  }

  /**
   * STEP 4
   * Copying...
   */

  separator();
  log(magenta('DO THE THING'));
  return true;
};

copy();
