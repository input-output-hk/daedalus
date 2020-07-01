// @flow

export const MIN_DELEGATION_FUNDS = 10;

// Dimensions

export const OFFSET_LEFT = 84;
export const OFFSET_TOP = 135;
export const THUMBNAIL_HEIGHT = 71;
export const THUMBNAIL_WIDTH = 80;
export const THUMBNAIL_OFFSET_WIDTH = THUMBNAIL_WIDTH / 2;
export const THUMBNAIL_OFFSET_HEIGHT = THUMBNAIL_HEIGHT / 2;
export const ARROW_WIDTH = 22;
export const ARROW_HEIGHT = 11;
export const ARROW_OFFSET = ARROW_WIDTH / 2;
export const TOOLTIP_DELTA = 5;
export const TOOLTIP_MIN_HEIGHT = 287;
export const TOOLTIP_MAX_HEIGHT = 370;
export const TOOLTIP_AVG_HEIGHT = (TOOLTIP_MIN_HEIGHT + TOOLTIP_MAX_HEIGHT) / 2;
export const TOOLTIP_WIDTH = 240;
export const CONTAINER_MARGIN = 21;
export const RECENT_STAKE_POOLS_COUNT = 6;

// Timers

export const STAKE_POOL_TRANSACTION_CHECK_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;
export const STAKE_POOL_TRANSACTION_CHECKER_TIMEOUT = 30 * 1000; // 30 seconds | unit: milliseconds;
export const STAKE_POOLS_INTERVAL = 1 * 60 * 1000; // 1 minute | unit: milliseconds;
export const STAKE_POOLS_FAST_INTERVAL = 1 * 1000; // 1 second | unit: milliseconds;
