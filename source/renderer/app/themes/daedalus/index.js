'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.EXISTING_THEME_OUTPUTS_OBJ = exports.EXISTING_THEME_OUTPUTS = void 0;
const lodash_1 = require('lodash');
const cardano_1 = require('./cardano');
const dark_blue_1 = require('./dark-blue');
const dark_cardano_1 = require('./dark-cardano');
const flight_candidate_1 = require('./flight-candidate');
const incentivized_testnet_1 = require('./incentivized-testnet');
const light_blue_1 = require('./light-blue');
const shelley_testnet_1 = require('./shelley-testnet');
const white_1 = require('./white');
const yellow_1 = require('./yellow');
exports.EXISTING_THEME_OUTPUTS = [
  ['cardano.ts', cardano_1.CARDANO_THEME_OUTPUT],
  ['dark-blue.ts', dark_blue_1.DARK_BLUE_THEME_OUTPUT],
  ['dark-cardano.ts', dark_cardano_1.DARK_CARDANO_THEME_OUTPUT],
  ['flight-candidate.ts', flight_candidate_1.FLIGHT_CANDIDATE_THEME_OUTPUT],
  [
    'incentivized-testnet.ts',
    incentivized_testnet_1.INCENTIVIZED_TESTNET_THEME_OUTPUT,
  ],
  ['light-blue.ts', light_blue_1.LIGHT_BLUE_THEME_OUTPUT],
  ['shelley-testnet.ts', shelley_testnet_1.SHELLEY_TESTNET_THEME_OUTPUT],
  ['white.ts', white_1.WHITE_THEME_OUTPUT],
  ['yellow.ts', yellow_1.YELLOW_THEME_OUTPUT],
];
exports.EXISTING_THEME_OUTPUTS_OBJ = exports.EXISTING_THEME_OUTPUTS.reduce(
  (outputsObj, theme) => {
    const [themeName, themeOutput] = theme;
    if (themeName && !(0, lodash_1.isEmpty)(themeOutput)) {
      // @ts-ignore ts-migrate(2538) FIXME: Type '{ aboutWindow: { '--theme-about-window-backg... Remove this comment to see the full error message
      outputsObj[themeName] = themeOutput;
    }
    return outputsObj;
  },
  {}
);
//# sourceMappingURL=index.js.map
