// @flow
import { createBackgroundShades, createErrorShades } from './createShades';

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

export const CREATE_THEME_MOCK_PARAMS = {
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
