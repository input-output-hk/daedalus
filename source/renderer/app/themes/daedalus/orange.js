// @flow
import { createTheme } from '../utils/createTheme';
import type { CreateThemeParams } from '../types';

const ORANGE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    error: '#ea4c5b',
    border: '#c6cdd6',
    focus: '#5e6066',
    background: {
      primary: '#efefef',
      secondary: '#ff8c1a',
    },
    text: {
      primary: '#5e6066',
      secondary: '#fafbfc',
    },
  },
};

export default createTheme(ORANGE_THEME_PARAMS);
