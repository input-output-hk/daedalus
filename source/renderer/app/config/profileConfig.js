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
  { value: 'comma-dot', label: '8,638,301,639.283542' },
  { value: 'dot-comma', label: '8.638.301.639,283542' },
  { value: 'space-dot', label: '8 638 301 639.283542' },
];

export const DATE_ENGLISH_OPTIONS = [
  { value: 'mm/dd/yyyy', label: 'mm/dd/yyyy' },
  { value: 'dd/mm/yyyy', label: 'dd/mm/yyyy' },
  { value: 'yyyy/mm/dd', label: 'yyyy/mm/dd' },
];

export const DATE_JAPANESE_OPTIONS = [
  { value: 'yyyy年mm月dd日', label: 'yyyy年mm月dd日' },
  { value: 'yy/mm/dd', label: 'yy/mm/dd' },
  { value: 'yyyy/mm/dd', label: 'yyyy/mm/dd' },
];

export const TIME_OPTIONS = [
  { value: '12-hour', label: '02:00 PM' },
  { value: '24-hour', label: '14:00' },
];
