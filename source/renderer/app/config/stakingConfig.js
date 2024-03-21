'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.EPOCH_COUNTDOWN_INTERVAL = exports.IS_STAKING_INFO_PAGE_AVAILABLE = exports.IS_SATURATION_DATA_AVAILABLE = exports.IS_RANKING_DATA_AVAILABLE = exports.IS_GRID_REWARDS_VIEW_AVAILABLE = exports.DELEGATION_ACTIONS = exports.DELEGATION_DEPOSIT = exports.REDEEM_ITN_REWARDS_STEPS = exports.REWARDS_REDEMPTION_FEE_CALCULATION_AMOUNT = exports.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE = exports.STAKE_POOLS_FETCH_TRACKER_CYCLES = exports.STAKE_POOLS_FETCH_TRACKER_INTERVAL = exports.STAKE_POOLS_FAST_INTERVAL = exports.STAKE_POOLS_INTERVAL = exports.STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT = exports.STAKE_POOL_TRANSACTION_CHECK_INTERVAL = exports.RECENT_STAKE_POOLS_COUNT = exports.CONTAINER_MARGIN = exports.TOOLTIP_WIDTH = exports.TOOLTIP_AVG_HEIGHT = exports.TOOLTIP_MAX_HEIGHT = exports.TOOLTIP_MIN_HEIGHT = exports.LIST_VIEW_ROW_HEIGHT = exports.LIST_VIEW_TOOLTIP_DELTA_TOP = exports.TOOLTIP_DELTA = exports.ARROW_OFFSET = exports.ARROW_HEIGHT = exports.ARROW_WIDTH = exports.THUMBNAIL_OFFSET_HEIGHT = exports.THUMBNAIL_OFFSET_WIDTH = exports.THUMBNAIL_WIDTH = exports.THUMBNAIL_HEIGHT = exports.OFFSET_TOP = exports.OFFSET_LEFT = exports.ALL_WALLETS_SELECTION_ID = exports.CIRCULATING_SUPPLY = exports.INITIAL_DESIRED_POOLS_NUMBER = exports.INITIAL_DELEGATION_FUNDS_LOG = exports.INITIAL_DELEGATION_FUNDS = exports.MIN_DELEGATION_FUNDS_LOG = exports.MIN_DELEGATION_FUNDS = exports.RANKING_SLIDER_RATIO = exports.SMASH_URL_VALIDATOR = exports.SMASH_SERVER_STATUSES = exports.SMASH_SERVER_INVALID_TYPES = exports.SMASH_SERVER_TYPES = exports.SMASH_SERVERS_LIST = void 0;
// @ts-ignore ts-migrate(2339) FIXME: Property 'smashUrl' does not exist on type 'typeof... Remove this comment to see the full error message
const { smashUrl, environment, isFlight } = global;
const { isMainnet } = environment;
// @ts-ignore ts-migrate(2739) FIXME: Type '{ iohk: { name: string; url: any; }; direct:... Remove this comment to see the full error message
exports.SMASH_SERVERS_LIST = {
  iohk: {
    name: 'IOHK',
    url: smashUrl,
  },
  // Metadata is fetched directly in URLs registered on chain,
  direct: {
    name: 'direct',
    url: 'direct',
  },
};
exports.SMASH_SERVER_TYPES = {
  IOHK: 'iohk',
  CUSTOM: 'custom',
  DIRECT: 'direct',
};
exports.SMASH_SERVER_INVALID_TYPES = {
  NONE: 'none',
};
exports.SMASH_SERVER_STATUSES = {
  AVAILABLE: 'available',
  UNAVAILABLE: 'unavailable',
  UNREACHABLE: 'unreachable',
  NO_SMASH_CONFIGURED: 'no_smash_configured',
};
exports.SMASH_URL_VALIDATOR = new RegExp(
  '^(direct|https://[a-zA-Z0-9-_~.]+(:[0-9]+)?/?)$'
);
exports.RANKING_SLIDER_RATIO = 60;
exports.MIN_DELEGATION_FUNDS = 10;
exports.MIN_DELEGATION_FUNDS_LOG = Math.log(exports.MIN_DELEGATION_FUNDS);
exports.INITIAL_DELEGATION_FUNDS = 1000;
exports.INITIAL_DELEGATION_FUNDS_LOG = Math.log(
  exports.INITIAL_DELEGATION_FUNDS
);
exports.INITIAL_DESIRED_POOLS_NUMBER = 150;
exports.CIRCULATING_SUPPLY = 33284769718;
exports.ALL_WALLETS_SELECTION_ID = '0';
// Dimensions
exports.OFFSET_LEFT = 84;
exports.OFFSET_TOP = 135;
exports.THUMBNAIL_HEIGHT = 71;
exports.THUMBNAIL_WIDTH = 80;
exports.THUMBNAIL_OFFSET_WIDTH = exports.THUMBNAIL_WIDTH / 2;
exports.THUMBNAIL_OFFSET_HEIGHT = exports.THUMBNAIL_HEIGHT / 2;
exports.ARROW_WIDTH = 22;
exports.ARROW_HEIGHT = 11;
exports.ARROW_OFFSET = exports.ARROW_WIDTH / 2;
exports.TOOLTIP_DELTA = 5;
exports.LIST_VIEW_TOOLTIP_DELTA_TOP = 120;
exports.LIST_VIEW_ROW_HEIGHT = 36;
exports.TOOLTIP_MIN_HEIGHT = 287;
exports.TOOLTIP_MAX_HEIGHT = 370;
exports.TOOLTIP_AVG_HEIGHT =
  (exports.TOOLTIP_MIN_HEIGHT + exports.TOOLTIP_MAX_HEIGHT) / 2;
exports.TOOLTIP_WIDTH = 240;
exports.CONTAINER_MARGIN = 21;
exports.RECENT_STAKE_POOLS_COUNT = 6;
// Timers
exports.STAKE_POOL_TRANSACTION_CHECK_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds
exports.STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT = 30 * 1000; // 30 seconds | unit: milliseconds
exports.STAKE_POOLS_INTERVAL = 1 * 60 * 1000; // 1 minute | unit: milliseconds
exports.STAKE_POOLS_FAST_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds
exports.STAKE_POOLS_FETCH_TRACKER_INTERVAL = 30 * 1000; // 30 seconds | unit: milliseconds
exports.STAKE_POOLS_FETCH_TRACKER_CYCLES = 6;
// Redeem ITN Rewards
exports.MIN_REWARDS_REDEMPTION_RECEIVER_BALANCE = 1; // 1 ADA | unit: ADA
exports.REWARDS_REDEMPTION_FEE_CALCULATION_AMOUNT = 1 * 1000000; // 1 ADA | unit: lovelace
exports.REDEEM_ITN_REWARDS_STEPS = {
  CONFIGURATION: 'configuration',
  CONFIRMATION: 'confirmation',
  RESULT: 'result',
};
exports.DELEGATION_DEPOSIT = 2; // 2 ADA | unit: lovelace
exports.DELEGATION_ACTIONS = {
  JOIN: 'join',
  QUIT: 'quit',
};
exports.IS_GRID_REWARDS_VIEW_AVAILABLE = !isMainnet && !isFlight;
exports.IS_RANKING_DATA_AVAILABLE = true;
exports.IS_SATURATION_DATA_AVAILABLE = true;
exports.IS_STAKING_INFO_PAGE_AVAILABLE = false;
exports.EPOCH_COUNTDOWN_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds
//# sourceMappingURL=stakingConfig.js.map
