export type BackgroundShades = Record<string, any>;
export type ErrorShades = Record<string, any>;
export type PendingThemesUpdates = {
  'cardano.ts'?: Record<string, any>;
  'dark-blue.ts'?: Record<string, any>;
  'dark-cardano.ts'?: Record<string, any>;
  'flight-candidate.ts'?: Record<string, any>;
  'light-blue.ts'?: Record<string, any>;
  'shelley-testnet.ts'?: Record<string, any>;
  'white.ts'?: Record<string, any>;
  'yellow.ts'?: Record<string, any>;
};
export type CreateThemeParams = {
  colors?: ColorParams;
  fonts?: ThemeFonts;
  config?: Record<string, any>;
};
export type ColorParams = {
  border: string;
  error: string;
  focus: string;
  background: {
    primary: string;
    secondary: string;
  };
  text: {
    primary: string;
    secondary: string;
  };
};
export type FindUpdatesParams = {
  cardano: Record<string, any>;
  darkBlue: Record<string, any>;
  darkCardano: Record<string, any>;
  flightCandidate: Record<string, any>;
  lightBlue: Record<string, any>;
  shelleyTestnet: Record<string, any>;
  yellow: Record<string, any>;
  white: Record<string, any>;
};
export type FormattedConstNames = {
  themeOutput: string;
  themeParams: string;
};
export type LogDifferencesParams = {
  color: string;
  missingDefs: Record<string, any>;
  themeName: string;
};
export type ThemeColors = {
  border: string;
  error: ErrorShades;
  focus: string;
  background: {
    primary: BackgroundShades;
    secondary: BackgroundShades;
  };
  text: {
    primary: string;
    secondary: string;
  };
};
export type ThemeFonts = {
  black: string;
  bold: string;
  heavy: string;
  light: string;
  medium: string;
  mono: string;
  regular: string;
  semibold: string;
  thin: string;
  ultralight: string;
};
export type WriteThemeUpdateParams = {
  fileName: string;
  updatedThemeObj: Record<string, any>;
};
