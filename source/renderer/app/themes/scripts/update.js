'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const lodash_1 = require('lodash');
const createTheme_1 = require('../utils/createTheme');
const findUpdates_1 = require('../utils/findUpdates');
const updateThemesCLI_1 = require('../utils/updateThemesCLI');
const constants_1 = require('../utils/constants');
const createThemeOutputs = constants_1.CREATE_THEME_PARAMS.reduce(
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'string | CreateThemeParams' is n... Remove this comment to see the full error message
  (outputs, theme) => [
    [theme[0], (0, createTheme_1.createTheme)(theme[1])],
    ...outputs,
  ],
  []
);
// @ts-ignore ts-migrate(2345) FIXME: Argument of type '(string | CreateThemeParams | (s... Remove this comment to see the full error message
const pendingUpdates = (0, findUpdates_1.findUpdates)(createThemeOutputs);
if (!(0, lodash_1.isEmpty)(pendingUpdates)) {
  // opens CLI which will allow user to update theme outputs in 'themes/daedalus'
  (0, updateThemesCLI_1.runUpdateThemesCLI)(pendingUpdates);
}
//# sourceMappingURL=update.js.map
