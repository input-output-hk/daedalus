// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

export const PURPLE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: '#ea4c5b',
    primary: {
      background: '#ccccff',
      border: '#b3b3ff',
      focus: '#330066',
      text: '#330066',
    },
    secondary: {
      background: '#9966ff',
      border: '#aa80ff',
      text: '#fafbfc',
    },
  },
};

export default createTheme(PURPLE_THEME_PARAMS);
