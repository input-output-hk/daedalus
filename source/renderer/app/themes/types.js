// @flow
export type BackgroundShades = Object;
export type ErrorShades = Object;

export type DaedalusThemesUpdates = {
  cardanoUpdates?: Object,
  darkBlueUpdates?: Object,
  lightBlueUpdates?: Object,
};

export type CreateThemeParams = {
  colors?: ColorParams,
  fonts?: ThemeFonts,
  config?: Object,
};

export type ColorParams = {
  border: string,
  error: string,
  focus: string,
  background: {
    primary: string,
    secondary: string,
  },
  text: {
    primary: string,
    secondary: string,
  },
};

export type FindUpdatesParams = {
  cardano: Object,
  darkBlue: Object,
  lightBlue: Object,
};

export type LogDifferencesParams = {
  color: string,
  missingDefs: Object,
  themeName: string,
};

export type ThemeColors = {
  border: string,
  error: ErrorShades,
  focus: string,
  background: {
    primary: BackgroundShades,
    secondary: BackgroundShades,
  },
  text: {
    primary: string,
    secondary: string,
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
