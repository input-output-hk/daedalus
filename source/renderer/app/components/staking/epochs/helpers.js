'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.humanizeDurationToShort = exports.sortData = exports.hasDataExisting = exports.noDataExisting = exports.getTableHeadersForPreviousEpoch = exports.getTableHeadersForCurrentEpoch = exports.SELECTED_EPOCH_OPTIONS = void 0;
const orderBy_1 = __importDefault(require('lodash/orderBy'));
const humanize_duration_1 = __importDefault(require('humanize-duration'));
exports.SELECTED_EPOCH_OPTIONS = {
  CURRENT_EPOCH: 'currentEpoch',
  PREVIOUS_EPOCH: 'previousEpoch',
};
const getTableHeadersForCurrentEpoch = (intl, messages) => [
  {
    name: 'pool',
    title: intl.formatMessage(messages.tableHeaderPool),
  },
  {
    name: 'slotsElected',
    title: intl.formatMessage(messages.tableHeaderSlotsElected),
  },
];
exports.getTableHeadersForCurrentEpoch = getTableHeadersForCurrentEpoch;
const getTableHeadersForPreviousEpoch = (intl, messages) => [
  {
    name: 'pool',
    title: intl.formatMessage(messages.tableHeaderPool),
  },
  {
    name: 'slotsElected',
    title: intl.formatMessage(messages.tableHeaderSlotsElected),
  },
  {
    name: 'performance',
    title: intl.formatMessage(messages.tableHeaderPerformance),
  },
  {
    name: 'sharedRewards',
    title: intl.formatMessage(messages.tableHeaderSharedRewards),
  },
];
exports.getTableHeadersForPreviousEpoch = getTableHeadersForPreviousEpoch;
const noDataExisting = (isLoading, data) =>
  !isLoading && ((data && !data.length) || !data);
exports.noDataExisting = noDataExisting;
const hasDataExisting = (isLoading, data) =>
  data && data.length > 0 && !isLoading;
exports.hasDataExisting = hasDataExisting;
const sortData = (data, order, sortBy) => {
  let realSortBy = '';
  if (sortBy === 'pool') {
    realSortBy = 'pool.name';
  } else if (sortBy === 'slotsElected') {
    realSortBy = 'slotsElected[0]';
  } else if (sortBy === 'sharedRewards') {
    realSortBy = 'sharedRewards[0]';
  } else if (sortBy === 'performance') {
    realSortBy = 'performance[2]';
  } else {
    realSortBy = sortBy;
  }
  return (0, orderBy_1.default)(data, realSortBy, order);
};
exports.sortData = sortData;
const humanizeDurationToShort = (currentLocale, dateTime) => {
  let humanizedDurationLanguage = null;
  switch (currentLocale) {
    case 'ja-JP':
      humanizedDurationLanguage = 'ja';
      break;
    default:
      humanizedDurationLanguage = 'en';
  }
  return (0, humanize_duration_1.default)(
    Math.max(0, new Date(dateTime).getTime() - new Date().getTime()),
    {
      round: true,
      language: humanizedDurationLanguage,
    }
  )
    .replace(/\s/g, '')
    .replace(/,/g, ' ');
};
exports.humanizeDurationToShort = humanizeDurationToShort;
//# sourceMappingURL=helpers.js.map
