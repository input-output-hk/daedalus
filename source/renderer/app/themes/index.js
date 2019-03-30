// @flow
export type ThemeColors = {
  primary: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  secondary: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  active: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  disabled: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  error: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  hover: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
  focus: {
    background: string,
    border: string,
    outline: string,
    placeholder: string,
    text: string,
  },
};

export type ThemeFonts = {
  ultralight: string,
  thin: string,
  light: string,
  regular: string,
  medium: string,
  semibold: string,
  bold: string,
  heavy: string,
  black: string,
  fallback: string,
};

export type ReactPolymorphTheme = {
  autocomplete: {
    '--rp-autocomplete-bg-color': string,
    '--rp-autocomplete-border': string,
    '--rp-autocomplete-border-color-opened': string,
    '--rp-autocomplete-input-text-color': string,
    '--rp-autocomplete-placeholder-color': string,
    '--rp-autocomplete-selected-word-box-bg-color': string,
    '--rp-autocomplete-selected-word-text-color': string,
    '--rp-autocomplete-selected-words-font-family': string,
  },
  bubble: {
    '--rp-bubble-bg-color': string,
    '--rp-bubble-border-color': string,
    '--rp-bubble-border-radius': string,
  },
  button: {
    '--rp-button-bg-color': string,
    '--rp-button-bg-color-active': string,
    '--rp-button-bg-color-disabled': string,
    '--rp-button-bg-color-hover': string,
    '--rp-button-font-family': string,
    '--rp-button-height': string,
    '--rp-button-line-height': string,
    '--rp-button-padding': string,
    '--rp-button-text-color': string,
    '--rp-button-text-transform': string,
    '--rp-button-width': string,
  },
  checkbox: {
    '--rp-checkbox-border': string,
    '--rp-checkbox-border-color-disabled': string,
    '--rp-checkbox-check-bg-color': string,
    '--rp-checkbox-label-text-color': string,
    '--rp-checkbox-label-text-color-disabled': string,
  },
  formfield: {},
  input: {},
  modal: {},
  options: {},
  select: {},
  switch: {},
  textarea: {},
};

export type DaedalusComponentsTheme = {};

export type DaedalusTheme = {};

export const THEMES = {
  CARDANO: 'cardano',
  DARK_BLUE: 'dark-blue',
  LIGHT_BLUE: 'light-blue',
};

export const createTheme = ({
  colors: ThemeColors,
  fonts: ThemeFonts,
  config: Object,
}): Object => {
  const { primary, secondary, active, disabled, error, hover, focus } = colors;
  // TODO: Use colors and fonts to create all css vars using color or font
  // then compose that object with the "config" parameter to create a full theme
};
