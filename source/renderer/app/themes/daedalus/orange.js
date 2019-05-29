// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

export const ORANGE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: '#ea4c5b',
    primary: {
      background: '#efefef',
      border: '#c6cdd6',
      focus: '#5e6066',
      text: '#5e6066',
    },
    secondary: {
      background: '#ff8c1a',
      border: '#efefef',
      text: '#fafbfc',
    },
  },
};

export default createTheme(ORANGE_THEME_PARAMS);
