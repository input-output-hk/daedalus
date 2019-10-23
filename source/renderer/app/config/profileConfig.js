import globalMessages from '../i18n/global-messages';

export const LANGUAGE_OPTIONS = [
  { value: 'en-US', label: globalMessages.languageEnglish },
  { value: 'ja-JP', label: globalMessages.languageJapanese },
  // { value: 'zh-CN', label: globalMessages.languageChinese },
  // { value: 'ko-KR', label: globalMessages.languageKorean },
  // { value: 'de-DE', label: globalMessages.languageGerman },
  // { value: 'hr-HR', label: globalMessages.languageCroatian },
];

export const NUMBER_OPTIONS = [
  { value: 'number-1', label: '8,638,301,639.283542' },
  { value: 'number-2', label: '8.638.301.639,283542' },
  { value: 'number-3', label: '8 638 301 639.283542' },
];

export const DATE_ENGLISH_OPTIONS = [
  { value: 'date-english-1', label: 'mm/dd/yyyy' },
  { value: 'date-english-2', label: 'dd/mm/yyyy' },
  { value: 'date-english-3', label: 'yyyy/mm/dd' },
];

export const DATE_JAPANESE_OPTIONS = [
  { value: 'date-japanese-1', label: 'yyyy年mm月dd日' },
  { value: 'date-japanese-2', label: 'yy/mm/dd' },
  { value: 'date-japanese-3', label: 'yyyy/mm/dd' },
];

export const TIME_OPTIONS = [
  { value: '12-hour', label: '02:00 PM' },
  { value: '24-hour', label: '14:00' },
];
