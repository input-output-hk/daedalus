'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const chalk_1 = __importDefault(require('chalk'));
const lodash_1 = require('lodash');
const inquirer_1 = __importDefault(require('inquirer'));
const index_1 = require('../daedalus/index');
const updateThemes_1 = require('./updateThemes');
const writeThemeUpdate_1 = require('./writeThemeUpdate');
// Config
const MAX_RESULTS_BEFORE_WARNING = 30;
const firstTheme = index_1.EXISTING_THEME_OUTPUTS[0][1];
const categories = (0, lodash_1.keys)(firstTheme);
const copy = async () => {
  let fromPrefix = '';
  let fromCategory = '';
  let foundProperties = [];
  let selectedProperties = [];
  let existingProperties = [];
  let toPrefix = '';
  let toCategory = '';
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
    fromPrefix = fromPrefix.trim();
    ({
      items: foundProperties,
      category: fromCategory,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string | { aboutWindow: { '--the... Remove this comment to see the full error message
    } = findPropertiesFromPrefix(firstTheme, fromPrefix));
    // No properties found
    if (!foundProperties.length) {
      warn(
        `I couldn't find any property with the prefix "${cyan(
          fromPrefix
        )}". Please, try again.`
      );
      return step1();
    }
    // Too many properties found (> MAX_RESULTS_BEFORE_WARNIN)
    if (foundProperties.length > MAX_RESULTS_BEFORE_WARNING) {
      const shouldProceed = await prompt({
        type: 'confirm',
        message: red(
          `I've found ${foundProperties.length} properties. Are you sure you want to proceed?`
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
      message: (0,
      chalk_1.default)`Great! I've found ${foundProperties.length} properties. Which ones should I copy over?\n`,
      choices: getChoicesFromProperties(foundProperties),
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
    toPrefix = toPrefix.trim();
    ({
      items: existingProperties,
      category: toCategory,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string | { aboutWindow: { '--the... Remove this comment to see the full error message
    } = findPropertiesFromPrefix(firstTheme, toPrefix));
    // Check existing properties with the given new prefix
    const conflictingProperties = existingProperties.filter(
      (existingProperty) => {
        const selectedProperty = replaceSingleProperty(
          existingProperty,
          toPrefix,
          fromPrefix
        );
        return !!selectedProperties.filter(
          (property) => property === selectedProperty
        ).length;
      }
    );
    // List and remove unselected conflicting properties
    if (conflictingProperties.length) {
      const confirmReplace = await prompt({
        type: 'checkbox',
        message:
          'The following properties already exist. Select which ones should be replaced.\n',
        choices: conflictingProperties,
      });
      const originalSelectedPropertiesLength = selectedProperties.length;
      selectedProperties = selectedProperties.filter((selectedProperty) => {
        const newProperty = replaceSingleProperty(
          selectedProperty,
          fromPrefix,
          toPrefix
        );
        return (
          !conflictingProperties.includes(newProperty) ||
          confirmReplace.includes(newProperty)
        );
      });
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
    let isNewCategory = false;
    if (!toCategory) {
      const categorType = await prompt({
        type: 'list',
        message: 'Which category do you want me to copy into?',
        choices: ['New category', 'Existing category'],
      });
      isNewCategory = categorType.includes('New');
      if (isNewCategory) {
        toCategory = await prompt({
          type: 'input',
          message: `What is the category name? (e.g. ${orange(
            'newFeatureName'
          )})'\n--->`,
        });
      } else {
        toCategory = await prompt({
          type: 'list',
          message: 'Choose an existing gategory',
          choices: getCategoriesChoices(),
        });
      }
    }
    const newProperties = selectedProperties.map((selectedProperty) =>
      replaceSingleProperty(selectedProperty, fromPrefix, toPrefix)
    );
    const confirmMessage = `Great, I'll copy ${
      selectedProperties.length
    } properties from "${cyan(fromPrefix)}" to "${cyan(
      toPrefix
    )}" into ${orange(toCategory)}.
Here's an example of how they will look like:
"{
  ...
  ${orange(toCategory)}: {${
      !isNewCategory
        ? `
    ...`
        : ''
    }
    ${newProperties
      .map((newProperty) => `${cyan(newProperty)}: ${magenta('...')},`)
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
    const pendingUpdates = index_1.EXISTING_THEME_OUTPUTS.reduce(
      (pending, [themeName, theme]) => {
        // @ts-ignore ts-migrate(2322) FIXME: Type '[string, unknown][]' is not assignable to ty... Remove this comment to see the full error message
        const fromProperties = (0, lodash_1.toPairs)(
          theme[fromCategory]
        ).filter(([propertyKey]) => selectedProperties.includes(propertyKey));
        const toProperties = fromProperties.map(
          ([fromPropertyKey, fromPropertyValue]) => [
            replaceSingleProperty(fromPropertyKey, fromPrefix, toPrefix),
            fromPropertyValue,
          ]
        );
        // @ts-ignore ts-migrate(2538) FIXME: Type '{ aboutWindow: { '--theme-about-window-backg... Remove this comment to see the full error message
        pending[themeName] = {};
        // @ts-ignore ts-migrate(2538) FIXME: Type '{ aboutWindow: { '--theme-about-window-backg... Remove this comment to see the full error message
        pending[themeName][toCategory] = toProperties.reduce(
          (obj, [propertyKey, propertyValue]) => {
            obj[propertyKey] = propertyValue;
            return obj;
          },
          {}
        );
        return pending;
      },
      {}
    );
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
    info(
      `\nGreat! I have finished adding the new properties and am running Prettier.\nMeanwhile you need to update the '${cyan(
        'CreateTheme.js'
      )}' file, because I can't do it automatically.\n`
    );
  };
  step1();
};
// Utils
const { log } = console;
const { cyan, red, magenta } = chalk_1.default;
const separator = () => log('\n');
const orange = (content) => chalk_1.default.keyword('orange')(content);
const info = (message) => log(orange(message));
const warn = (message) => log(red(message));
const prompt = async (promptConfig) => {
  separator();
  const { response } = await inquirer_1.default.prompt([
    { ...promptConfig, name: 'response' },
  ]);
  return response;
};
// Helpers
const getCategoriesChoices = () =>
  categories.map((category) => ({
    value: category,
    short: `\nOk, I'll use the existing ${orange(category)} category`,
    name: category,
  }));
const getChoicesFromProperties = (properties) =>
  properties.map((propertyName) => ({
    value: propertyName,
    short: `\n✔ ${propertyName}`,
    name: cyan(propertyName),
    checked: true,
  }));
const findPropertiesFromPrefix = (themeObj, prefix) =>
  (0, lodash_1.toPairs)(themeObj).reduce(
    (response, [categoryName, categoryObj]) => {
      const existingProperties = (0, lodash_1.keys)(categoryObj).filter(
        (property) => property.indexOf(prefix) > -1
      );
      if (existingProperties.length) {
        if (!response.category) response.category = categoryName;
        response.items = existingProperties;
      }
      return response;
    },
    {
      category: '',
      items: [],
    }
  );
const replaceSingleProperty = (propertyName, fromPrefix, toPrefix) => {
  const fromPrefixNoDash = removeLastDash(fromPrefix);
  const toPrefixNoDash = removeLastDash(toPrefix);
  return propertyName.replace(fromPrefixNoDash, toPrefixNoDash);
};
const removeLastDash = (property) => {
  let prop = property;
  if (property.slice(-1) === '-') prop = prop.replace(/-$/, '');
  return prop;
};
copy();
//# sourceMappingURL=copyTheme.js.map
