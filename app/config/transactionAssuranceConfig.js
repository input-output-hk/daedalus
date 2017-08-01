// @flow
import type {
  AssuranceMode, AssuranceLevel, AssuranceModeOption,
} from '../types/transactionAssuranceTypes';

export const assuranceModeOptions: {
  NORMAL: AssuranceModeOption, STRICT: AssuranceModeOption,
} = {
  NORMAL: 'CWANormal', STRICT: 'CWAStrict',
};

export const assuranceModes: { NORMAL: AssuranceMode, STRICT: AssuranceMode } = {
  NORMAL: {
    low: 2,
    medium: 6,
  },
  STRICT: {
    low: 4,
    medium: 10,
  }
};

export const assuranceLevels: {
  LOW: AssuranceLevel, MEDIUM: AssuranceLevel, HIGH: AssuranceLevel,
} = {
  LOW: 'low', MEDIUM: 'medium', HIGH: 'high',
};
