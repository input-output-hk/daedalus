// @flow
export type BackgroundShades = Object;
export type ErrorShades = Object;

export type PendingThemesUpdates = {
  'cardano.ts'?: Object,
  'dark-blue.ts'?: Object,
  'dark-cardano.ts'?: Object,
  'flight-candidate.ts'?: Object,
  'light-blue.ts'?: Object,
  'shelley-testnet.ts'?: Object,
  'white.ts'?: Object,
  'yellow.ts'?: Object,
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
  darkCardano: Object,
  flightCandidate: Object,
  lightBlue: Object,
  shelleyTestnet: Object,
  yellow: Object,
  white: Object,
};

export type FormattedConstNames = {
  themeOutput: string,
  themeParams: string,
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

export type WriteThemeUpdateParams = {
  fileName: string,
  updatedThemeObj: Object,
};
