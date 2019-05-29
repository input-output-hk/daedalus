// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

export const TURQUOISE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: '#ea4c5b',
    primary: {
      background: '#efefef',
      border: '#c6cdd6',
      focus: '#5e6066',
      text: '#5e6066',
    },
    secondary: {
      background: '#006666',
      border: '#efefef',
      text: '#fafbfc',
    },
  },
};

export default createTheme(TURQUOISE_THEME_PARAMS);
