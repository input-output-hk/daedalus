export type Locale = 'en-US' | 'ja-JP';
export const LOCALES: {
  english: Locale;
  japanese: Locale;
} = {
  english: 'en-US',
  japanese: 'ja-JP',
};
export const humanizedDurationLanguages = {
  'en-US': 'en',
  'ja-JP': 'ja',
};
export const momentLocales: Record<Locale, string> = {
  'en-US': 'en-us',
  'ja-JP': 'ja',
};
