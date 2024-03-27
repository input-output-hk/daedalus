'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getUrlParts = exports.getSmashServerIdFromUrl = exports.getSmashServerNameFromUrl = void 0;
const lodash_1 = require('lodash');
const stakingConfig_1 = require('../config/stakingConfig');
const getSmashServerNameFromUrl = (smashServerUrl) =>
  (0, lodash_1.reduce)(
    stakingConfig_1.SMASH_SERVERS_LIST,
    (result, { name, url }) => {
      if (url === smashServerUrl) result = name;
      return result;
    },
    smashServerUrl
  );
exports.getSmashServerNameFromUrl = getSmashServerNameFromUrl;
const getSmashServerIdFromUrl = (smashServerUrl) =>
  (0, lodash_1.reduce)(
    stakingConfig_1.SMASH_SERVERS_LIST,
    (result, { url }, id) => {
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'SmashServ... Remove this comment to see the full error message
      if (url === smashServerUrl) result = id;
      return result;
    },
    stakingConfig_1.SMASH_SERVER_TYPES.CUSTOM
  );
exports.getSmashServerIdFromUrl = getSmashServerIdFromUrl;
const getUrlParts = (url) => {
  try {
    return new URL(url);
  } catch (error) {
    // @ts-ignore ts-migrate(2740) FIXME: Type '{}' is missing the following properties from... Remove this comment to see the full error message
    return {};
  }
};
exports.getUrlParts = getUrlParts;
//# sourceMappingURL=staking.js.map
