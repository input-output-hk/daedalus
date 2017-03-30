// @flow
import { PropTypes } from 'react';

// PropTypes

export const AssuranceModePropType = PropTypes.shape({
  low: PropTypes.number.isRequired,
  medium: PropTypes.number.isRequired,
});

// Flow Types

export type AssuranceModeOption = 'CWANormal' | 'CWAStrict';
export type AssuranceMode = { low: number, medium: number };
export type AssuranceLevel = 'low' | 'medium' | 'high';

