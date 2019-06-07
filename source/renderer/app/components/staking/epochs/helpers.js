import { orderBy } from 'lodash';

export const getTableHeadersForCurrentEpoch = (intl, messages) => [
  {
    name: 'pool',
    title: intl.formatMessage(messages.tableHeaderPool),
  },
  {
    name: 'slotsElected',
    title: intl.formatMessage(messages.tableHeaderSlotsElected),
  },
];

export const getTableHeadersForPreviousEpoch = (intl, messages) => [
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

export const noDataExisting = (isLoading, data) =>
  !isLoading && ((data && !data.length) || !data);

export const hasDataExisting = (isLoading, data) =>
  data && data.length > 0 && !isLoading;

export const sortData = (data, order, sortBy) =>
  orderBy(data, sortBy === 'pool' ? 'pool.title' : sortBy, order);
