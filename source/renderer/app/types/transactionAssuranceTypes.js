// @flow
export type AssuranceModeOption = 'CWANormal' | 'CWAStrict';
export type AssuranceMode = { low: number, medium: number };
export type AssuranceLevel = 'low' | 'medium' | 'high';

export const assuranceModeOptions: {
  NORMAL: AssuranceModeOption, STRICT: AssuranceModeOption,
} = {
  NORMAL: 'CWANormal', STRICT: 'CWAStrict',
};

export const assuranceModes: { NORMAL: AssuranceMode, STRICT: AssuranceMode } = {
  NORMAL: {
    low: 3,
    medium: 9,
  },
  STRICT: {
    low: 5,
    medium: 15,
  }
};

export const assuranceLevels: {
  LOW: AssuranceLevel, MEDIUM: AssuranceLevel, HIGH: AssuranceLevel,
} = {
  LOW: 'low', MEDIUM: 'medium', HIGH: 'high',
};
