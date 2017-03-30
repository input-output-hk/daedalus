// @flow
import type {
  AssuranceMode, AssuranceLevel, AssuranceModeOption,
} from '../types/transactionAssuranceTypes';

export const assuranceModeOptions: {
  NORMAL: AssuranceModeOption, STRICT: AssuranceModeOption,
} = {
  NORMAL: 'normal', STRICT: 'strict',
};

export const assuranceModes: { NORMAL: AssuranceMode, STRICT: AssuranceMode } = {
  NORMAL: {
    low: 3,
    medium: 7,
  },
  STRICT: {
    low: 5,
    medium: 11,
  }
};

export const assuranceLevels: {
  LOW: AssuranceLevel, MEDIUM: AssuranceLevel, HIGH: AssuranceLevel,
} = {
  LOW: 'low', MEDIUM: 'medium', HIGH: 'high',
};
