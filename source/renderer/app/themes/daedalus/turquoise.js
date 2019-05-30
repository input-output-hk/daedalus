// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

const TURQUOISE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: '#ea4c5b',
    border: '#c6cdd6',
    focus: '#5e6066',
    background: {
      primary: '#efefef',
      secondary: '#006666',
    },
    text: {
      primary: '#5e6066',
      secondary: '#fafbfc',
    },
  },
};

export default createTheme(TURQUOISE_THEME_PARAMS);
