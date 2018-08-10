// @flow
export type AssuranceModeOption = 'normal' | 'strict';
export type AssuranceMode = { low: number, medium: number };
export type AssuranceLevel = 'low' | 'medium' | 'high';

// TODO Remove after v1 implementation is complete
export type AssuranceModeOptionV0 = 'CWANormal' | 'CWAStrict';
// TODO Remove after v1 implementation is complete
export const assuranceModeOptionsV0: {
  NORMAL: AssuranceModeOptionV0, STRICT: AssuranceModeOptionV0,
} = {
  NORMAL: 'CWANormal', STRICT: 'CWAStrict',
};

export const assuranceModeOptions: {
  NORMAL: AssuranceModeOption, STRICT: AssuranceModeOption,
} = {
  NORMAL: 'normal', STRICT: 'strict',
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
