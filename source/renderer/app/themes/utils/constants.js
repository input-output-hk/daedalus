// @flow
import { createBackgroundShades, createErrorShades } from './createShades';
import {
  createReactPolymorphTheme,
  createDaedalusComponentsTheme,
} from './createTheme';
import type { PartialThemeParts } from './createTheme';
import type { CreateThemeParams } from '../types';

export const DEFAULT_FONTS = {
  black: 'NotoSans-Black, NotoSansCJKjp-Black',
  bold: 'NotoSans-Bold, NotoSansCJKjp-Bold',
  heavy: 'NotoSans-ExtraBold, NotoSansCJKjp-Black',
  light: 'NotoSans-Light, NotoSansCJKjp-Light',
  medium: 'NotoSans-Medium, NotoSansCJKjp-Medium',
  mono: 'SFMono-Light',
  regular: 'NotoSans-Regular, NotoSansCJKjp-Regular',
  semibold: 'NotoSans-SemiBold, NotoSansCJKjp-Medium',
  thin: 'NotoSans-Thin, NotoSansCJKjp-Thin',
  ultralight: 'NotoSans-ExtraLight, NotoSansCJKjp-Thin',
};

export const CREATE_THEME_MOCK_PARAMS: PartialThemeParts = {
  colors: {
    border: '#eee',
    error: createErrorShades('#eee'),
    focus: '#eee',
    background: {
      primary: createBackgroundShades('#eee'),
      secondary: createBackgroundShades('#eee'),
    },
    text: {
      primary: '#eee',
      secondary: '#eee',
    },
  },
  fonts: DEFAULT_FONTS,
};

export const CREATE_THEME_OBJ = {
  ...createReactPolymorphTheme(CREATE_THEME_MOCK_PARAMS),
  ...createDaedalusComponentsTheme(CREATE_THEME_MOCK_PARAMS),
};

export const CREATE_CARDANO_THEME_PARAMS: CreateThemeParams = {
  colors: {
    background: {
      primary: '#efefef',
      secondary: '#2cbb69',
    },
    border: '#c6cdd6',
    error: '#ea4c5b',
    focus: '#5e6066',
    text: {
      primary: '#5e6066',
      secondary: '#fafbfc',
    },
  },
  fonts: DEFAULT_FONTS,
};

export const CREATE_LIGHT_BLUE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    background: {
      primary: '#ebeff2',
      secondary: '#2f496e',
    },
    border: '#c6cdd6',
    error: '#ea4c5b',
    focus: '#5e6066',
    text: {
      primary: '#5e6066',
      secondary: '#fafbfc',
    },
  },
  fonts: DEFAULT_FONTS,
};

export const CREATE_DARK_BLUE_THEME_PARAMS: CreateThemeParams = {
  colors: {
    background: {
      primary: '#263345',
      secondary: '#536370',
    },
    border: 'rgba(102, 122, 138, 0.3)',
    error: '#ea4c5b',
    focus: '#667a8a',
    text: {
      primary: '#e9f4fe',
      secondary: '#fafbfc',
    },
  },
  fonts: DEFAULT_FONTS,
};
