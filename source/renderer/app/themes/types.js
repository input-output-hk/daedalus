// @flow
export type BackgroundShades = Object;
export type ErrorShades = Object;

// export type BackgroundShades = {
//   lightest: string,
//   lighter: string,
//   light: string,
//   regular: string,
//   dark: string,
//   darker: string,
//   darkest: string,
// };

// export type ErrorShades = {
//   ultralight: string,
//   lightest: string,
//   lighter: string,
//   light: string,
//   regular: string,
//   dark: string,
//   darker: string,
//   darkest: string,
// };

export type CreateThemeParams = {
  colors: ColorParams,
  fonts: ThemeFonts,
  config?: Object,
};

export type ColorParams = {
  error: string,
  primary: {
    background: string,
    border: string,
    focus: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  secondary: {
    background: string,
    border: string,
    focus: string,
    hover: string,
    outline: string,
    placeholder: string,
    text: string,
  },
};

export type ThemeColors = {
  error: ErrorShades,
  primary: {
    background: BackgroundShades,
    border: string,
    focus: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  secondary: {
    background: BackgroundShades,
    border: string,
    focus: string,
    hover: string,
    outline: string,
    placeholder: string,
    text: string,
  },
};

export type ThemeFonts = {
  black: string,
  bold: string,
  heavy: string,
  light: string,
  medium: string,
  mono: string,
  regular: string,
  semibold: string,
  thin: string,
  ultralight: string,
};
