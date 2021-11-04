import globalMessages from '../i18n/global-messages';

export const PROFILE_SETTINGS = [
  'locale',
  'numberFormat',
  'dateFormat',
  'timeFormat',
];

export const LANGUAGE_OPTIONS = [
  { value: 'en-US', label: globalMessages.languageEnglish },
  { value: 'ja-JP', label: globalMessages.languageJapanese },
];

export const NUMBER_OPTIONS = [
  { value: 'number-1', label: '8,638,301,639.283542' },
  { value: 'number-2', label: '8.638.301.639,283542' },
  { value: 'number-3', label: '8 638 301 639.283542' },
];

export const DATE_ENGLISH_OPTIONS = [
  { value: 'MM/DD/YYYY', label: 'mm/dd/yyyy' },
  { value: 'DD/MM/YYYY', label: 'dd/mm/yyyy' },
  { value: 'YYYY/MM/DD', label: 'yyyy/mm/dd' },
];

export const DATE_JAPANESE_OPTIONS = [
  { value: 'YYYY年MM月DD日', label: 'yyyy年mm月dd日' },
  { value: 'YY/MM/DD', label: 'yy/mm/dd' },
  { value: 'YYYY/MM/DD', label: 'yyyy/mm/dd' },
];

export const TIME_OPTIONS = [
  { value: 'hh:mm:ss A', label: '02:00 PM' },
  { value: 'HH:mm:ss', label: '14:00' },
];

export const DATE_ENGLISH_LL_MAP_OPTIONS = {
  [DATE_ENGLISH_OPTIONS[0].value]: 'MMM D, YYYY',
  [DATE_ENGLISH_OPTIONS[1].value]: 'D MMM, YYYY',
  [DATE_ENGLISH_OPTIONS[2].value]: 'YYYY, MMM D',
};

export const TIME_LL_MAP_OPTIONS = {
  [TIME_OPTIONS[0].value]: 'hh:mm A',
  [TIME_OPTIONS[1].value]: 'HH:mm',
};
