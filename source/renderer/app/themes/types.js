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
//   light: string,
//   regular: string,
//   dark: string,
// };

export type CreateThemeParams = {
  colors: ColorParams,
  fonts: ThemeFonts,
  config?: Object,
};

export type ColorParams = {
  // error: {
  //   dark: string,
  //   light: string,
  //   regular: string,
  //   ultralight: string,
  // },
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

// TODO: Finish ReactPolymorphTheme definition
// export type ReactPolymorphTheme = {
//   autocomplete: {
//     '--rp-autocomplete-bg-color': string,
//     '--rp-autocomplete-border': string,
//     '--rp-autocomplete-border-color-opened': string,
//     '--rp-autocomplete-input-text-color': string,
//     '--rp-autocomplete-placeholder-color': string,
//     '--rp-autocomplete-selected-word-box-bg-color': string,
//     '--rp-autocomplete-selected-word-text-color': string,
//     '--rp-autocomplete-selected-words-font-family': string,
//   },
//   bubble: {
//     '--rp-bubble-bg-color': string,
//     '--rp-bubble-border-color': string,
//     '--rp-bubble-border-radius': string,
//   },
//   button: {
//     '--rp-button-bg-color': string,
//     '--rp-button-bg-color-active': string,
//     '--rp-button-bg-color-disabled': string,
//     '--rp-button-bg-color-hover': string,
//     '--rp-button-font-family': string,
//     '--rp-button-height': string,
//     '--rp-button-line-height': string,
//     '--rp-button-padding': string,
//     '--rp-button-text-color': string,
//     '--rp-button-text-transform': string,
//     '--rp-button-width': string,
//   },
//   checkbox: {
//     '--rp-checkbox-border': string,
//     '--rp-checkbox-border-color-disabled': string,
//     '--rp-checkbox-check-bg-color': string,
//     '--rp-checkbox-label-text-color': string,
//     '--rp-checkbox-label-text-color-disabled': string,
//   },
//   formfield: {},
//   input: {},
//   modal: {},
//   options: {},
//   select: {},
//   switch: {},
//   textarea: {},
// };
