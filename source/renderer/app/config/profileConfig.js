'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.DATE_TIME_SEPARATOR_MAP = exports.TIME_LL_MAP_OPTIONS = exports.DATE_ENGLISH_LL_MAP_OPTIONS = exports.TIME_OPTIONS = exports.DATE_JAPANESE_OPTIONS = exports.DATE_ENGLISH_OPTIONS = exports.NUMBER_OPTIONS = exports.LANGUAGE_OPTIONS = exports.PROFILE_SETTINGS = void 0;
const global_messages_1 = __importDefault(require('../i18n/global-messages'));
exports.PROFILE_SETTINGS = [
  'locale',
  'numberFormat',
  'dateFormat',
  'timeFormat',
];
exports.LANGUAGE_OPTIONS = [
  {
    value: 'en-US',
    label: global_messages_1.default.languageEnglish,
  },
  {
    value: 'ja-JP',
    label: global_messages_1.default.languageJapanese,
  },
];
exports.NUMBER_OPTIONS = [
  {
    value: 'number-1',
    label: '8,638,301,639.283542',
  },
  {
    value: 'number-2',
    label: '8.638.301.639,283542',
  },
  {
    value: 'number-3',
    label: '8 638 301 639.283542',
  },
];
exports.DATE_ENGLISH_OPTIONS = [
  {
    value: 'MM/DD/YYYY',
    label: 'mm/dd/yyyy',
  },
  {
    value: 'DD/MM/YYYY',
    label: 'dd/mm/yyyy',
  },
  {
    value: 'YYYY/MM/DD',
    label: 'yyyy/mm/dd',
  },
];
exports.DATE_JAPANESE_OPTIONS = [
  {
    value: 'YYYY年MM月DD日',
    label: 'yyyy年mm月dd日',
  },
  {
    value: 'YY/MM/DD',
    label: 'yy/mm/dd',
  },
  {
    value: 'YYYY/MM/DD',
    label: 'yyyy/mm/dd',
  },
];
exports.TIME_OPTIONS = [
  {
    value: 'hh:mm:ss A',
    label: '02:00 PM',
  },
  {
    value: 'HH:mm:ss',
    label: '14:00',
  },
];
exports.DATE_ENGLISH_LL_MAP_OPTIONS = {
  ['MM/DD/YYYY']: 'MMM D, YYYY',
  ['DD/MM/YYYY']: 'D MMM, YYYY',
  ['YYYY/MM/DD']: 'YYYY, MMM D',
};
exports.TIME_LL_MAP_OPTIONS = {
  ['hh:mm:ss A']: 'hh:mm A',
  ['HH:mm:ss']: 'HH:mm',
};
exports.DATE_TIME_SEPARATOR_MAP = new Proxy(
  {
    ['YYYY年MM月DD日']: '',
    ['YY/MM/DD']: ' ',
    ['YYYY/MM/DD']: ' ',
  },
  {
    get: (target, prop, receiver) => {
      const notSpecified = !(prop in target);
      if (notSpecified) {
        return ', ';
      }
      return Reflect.get(target, prop, receiver);
    },
  }
);
//# sourceMappingURL=profileConfig.js.map
