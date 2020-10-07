// @flow
import chalk from 'chalk';
import inquirer from 'inquirer';
import { EXISTING_THEME_OUTPUTS } from '../daedalus/index';
import { runUpdateThemesCLI } from './updateThemesCLI';

// Config
const MAX_RESULTS_BEFORE_WARNING = 30;
const firstTheme = EXISTING_THEME_OUTPUTS[0][1];
const categories = Object.keys(firstTheme);

// Types
type Property = Array<string>;

const copy = async () => {
  let fromPrefix;
  let properties;
  let selectedProperties;
  let toPrefix;
  let fromCategory;
  let existingProperties;
  let existingCategory;

  /**
   * STEP 1
   * Get the properties prefix.
   * E.g. "--theme-wallet-import-..."
   */
  const step1 = async () => {
    fromPrefix = await prompt({
      type: 'input',
      message: `What is the property prefix?\ne.g. ${cyan(
        '--theme-wallet-import-'
      )}\n--->`,
    });

    if (!fromPrefix) {
      warn('Invalid prefix');
      return step1();
    }

    ({ items: properties, category: fromCategory } = findPropertiesFromPrefix(
      firstTheme,
      fromPrefix
    ));

    // No properties found
    if (!properties.length) {
      warn(
        `I couldn't find any property with the prefix "${cyan(
          fromPrefix
        )}". Please, try again.`
      );
      return step1();
    }

    // Too many properties found (> MAX_RESULTS_BEFORE_WARNIN)
    if (properties.length > MAX_RESULTS_BEFORE_WARNING) {
      const shouldProceed = await prompt({
        type: 'confirm',
        message: red(
          `I've found over ${properties.length} properties. Are you sure you want to proceed?`
        ),
      });
      if (!shouldProceed) {
        info(`Ok, restarting...`);
        return step1();
      }
    }
    return step2();
  };

  /**
   * STEP 2
   * Select properties to be copied over
   */
  const step2 = async () => {
    selectedProperties = await prompt({
      type: 'checkbox',
      message: chalk`Great! I've found ${properties.length} properties. Which ones should I copy over?\n`,
      choices: getChoicesFromProperties(properties),
      loop: false,
    });

    if (!selectedProperties.length) {
      warn('No properties were selected...');
      return step2();
    }
    return step3();
  };

  /**
   * STEP 3
   * New prefix to copy into
   */
  const step3 = async () => {
    toPrefix = await prompt({
      type: 'input',
      message: `What is the new property prefix I should copy into?\ne.g. ${cyan(
        '--theme-new-super-feature-'
      )}\n--->`,
    });

    if (!toPrefix || fromPrefix === toPrefix) {
      warn('Invalid prefix');
      return step3();
    }

    ({
      items: existingProperties,
      category: existingCategory,
    } = findPropertiesFromPrefix(firstTheme, toPrefix));

    // Check existing properties with the given new prefix
    const conflictingProperties = existingProperties.reduce(
      (conflicting, [propertyKey]) => {
        const selectedPropertyKey = replaceSingleProperty(
          propertyKey,
          toPrefix,
          fromPrefix
        );
        if (
          selectedProperties.filter(prop => prop[0] === selectedPropertyKey)
            .length
        )
          conflicting.push(propertyKey);
        return conflicting;
      },
      []
    );

    // List and remove unselected conflicting properties
    if (conflictingProperties.length) {
      const confirmReplace = await prompt({
        type: 'checkbox',
        message:
          'The following properties already exist. Select which ones should be replaced.',
        choices: conflictingProperties,
      });
      const originalSelectedPropertiesLength = selectedProperties.length;
      selectedProperties = selectedProperties.reduce((list, category) => {
        let [categoryKey] = category;
        categoryKey = replaceSingleProperty(categoryKey, fromPrefix, toPrefix);
        if (
          !conflictingProperties.includes(categoryKey) ||
          confirmReplace.includes(categoryKey)
        )
          list.push(category);
        return list;
      }, []);
      const removed =
        originalSelectedPropertiesLength - selectedProperties.length;
      if (removed > 0) {
        separator();
        log(`Great, I've removed ${removed} existing properties`);
      }
    }

    return step4();
  };

  /**
   * STEP 4
   * Category to copy the properties into
   */
  const step4 = async () => {
    let category;
    let isNewCategory = false;
    if (existingCategory) category = existingCategory;
    else {
      const categorType = await prompt({
        type: 'list',
        message: 'Which category do you want me to copy into?',
        choices: ['New category', 'Existing category'],
      });

      isNewCategory = categorType.includes('New');
      if (isNewCategory) {
        category = await prompt({
          type: 'input',
          message: `What is the category name? (e.g. ${orange(
            'newFeatureName'
          )})'\n--->`,
        });
      } else {
        category = await prompt({
          type: 'list',
          message: 'Choose an existing gategory',
          choices: getCategoriesChoices(),
        });
      }
    }

    const newProperties = selectedProperties.map(([propKey, propValue]) => [
      replaceSingleProperty(propKey, fromPrefix, toPrefix),
      propValue,
    ]);

    const confirmMessage = `Great, I'll copy ${
      selectedProperties.length
    } properties from "${cyan(fromPrefix)}" to "${cyan(
      toPrefix
    )}" into ${orange(category)}.
Here's an example of how they will look like:
"{
  ...
  ${orange(category)}: {${
      !isNewCategory
        ? `
    `
        : ''
    }
    ${newProperties
      .map(([key, value]) => `${cyan(key)}: ${magenta(value)},`)
      .join(`\n    `)}
  },
  ...
}"
Should I proceed?
`;

    const confirmCopy = await prompt({
      type: 'confirm',
      message: confirmMessage,
    });
    if (!confirmCopy) {
      info(`Ok, restarting...`);
      return step1();
    }

    return step5();
  };

  /**
   * STEP 5
   * Copying...
   */
  const step5 = async () => {
    // log(magenta('DO THE THING'));
    info(`Great, I'll run the theme updater:`);

    // const newProperties = selectedProperties.map(([propKey, propValue]) => [
    //   replaceSingleProperty(propKey, fromPrefix, toPrefix),
    //   propValue,
    // ]);

    // const pendingUpdates = EXISTING_THEME_OUTPUTS.map(
    //   ([themeName, theme]) => {

    //     // const fromProperties =

    //   }
    // );

    // runUpdateThemesCLI(pendingUpdates);
  };

  step1();
};

// Utils
const { log } = console;
const { cyan, red, magenta } = chalk;
const separator = () => log('\n');
const orange = content => chalk.keyword('orange')(content);
const info = message => log(orange(message));
const warn = message => log(red(message));
const prompt = async promptConfig => {
  separator();
  const { response } = await inquirer.prompt([
    {
      ...promptConfig,
      name: 'response',
    },
  ]);
  return response;
};

// Helpers
const getCategoriesChoices = () =>
  categories.map(category => ({
    value: category,
    short: `\nOk, I'll use the existing ${orange(category)} category`,
    name: category,
  }));

const getChoicesFromProperties = properties =>
  properties.map(([name, value]) => ({
    value: [name, value],
    short: `\n✔ ${name}`,
    name: `${cyan(name)}: ${magenta(value)}`,
    checked: true,
  }));

const findPropertiesFromPrefix = (
  themeObj: Object,
  prefix: string
): {
  category: string,
  items: Array<any>,
} =>
  Object.entries(themeObj).reduce(
    (response, [categoryName, categoryObj]) => {
      const hasExistingProperties = Object.keys(categoryObj).filter(
        property => property.indexOf(prefix) > -1
      );
      if (hasExistingProperties.length) {
        if (!response.category) response.category = categoryName;
        response.items = Object.entries(categoryObj);
      }
      return response;
    },
    {
      category: '',
      items: [],
    }
  );

const replaceSingleProperty = (
  propertyName: string,
  fromPrefix: string,
  toPrefix
) => {
  const fromPrefixNoDash = removeLastDash(fromPrefix);
  const toPrefixNoDash = removeLastDash(toPrefix);
  return propertyName.replace(fromPrefixNoDash, toPrefixNoDash);
};

const removeLastDash = property => {
  let prop = property;
  if (property.slice(-1) === '-') prop = prop.replace(/-$/, '');
  return prop;
};

copy();
