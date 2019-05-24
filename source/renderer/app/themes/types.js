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
    active: string,
    background: string,
    border: string,
    disabled: string,
    focus: string,
    hover: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  secondary: {
    active: string,
    background: string,
    border: string,
    disabled: string,
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
    active: string,
    background: BackgroundShades,
    border: string,
    disabled: string,
    focus: string,
    hover: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  secondary: {
    active: string,
    background: BackgroundShades,
    border: string,
    disabled: string,
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
