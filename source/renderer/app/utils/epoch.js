// @flow
import type { SlotLength } from '../api/network/types';

const SLOT_LENGTH_UNITS = {
  MILLISECOND: 'millisecond',
  SECOND: 'second',
  MINUTE: 'minute',
  HOUR: 'hour',
  DAY: 'day',
};

const SLOT_LENGTH_UNIT_MAP = {
  [SLOT_LENGTH_UNITS.MILLISECOND]: 1,
  [SLOT_LENGTH_UNITS.SECOND]: 1000,
  [SLOT_LENGTH_UNITS.MINUTE]: 60 * 1000,
  [SLOT_LENGTH_UNITS.HOUR]: 3600 * 1000,
  [SLOT_LENGTH_UNITS.DAY]: 24 * 3600 * 1000,
};

export const generateEpochCountdownInterval = (slotLength: ?SlotLength) => {
  if (!slotLength) {
    return null;
  }

  return SLOT_LENGTH_UNIT_MAP[slotLength.unit];
};
