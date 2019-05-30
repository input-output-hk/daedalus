// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

const PURPLE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: '#ea4c5b',
    border: '#b3b3ff',
    focus: '#330066',
    background: {
      primary: '#ccccff',
      secondary: '#9966ff',
    },
    text: {
      primary: '#330066',
      secondary: '#fafbfc',
    },
  },
};

export default createTheme(PURPLE_THEME_PARAMS);
