'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const humanize_duration_1 = __importDefault(require('humanize-duration'));
const lodash_1 = require('lodash');
const locales_types_1 = require('../../../common/types/locales.types');
const generalConfig = {
  round: true,
  // round seconds to prevent e.g. 1 day 3 hours *11,56 seconds*
  units: ['d', 'h', 'm'],
};
const localeConfigs = {
  'en-US': {
    conjunction: ' and ',
    serialComma: false,
  },
  'ja-JP': {
    language: 'customJapanese',
    spacer: '',
    delimiter: '',
    languages: {
      customJapanese: {
        d: () => '日と',
        h: () => '時間',
        m: () => '分',
      },
    },
  },
};
exports.default = (time, currentLocale, configOverride = {}) => {
  const customLocaleConfig = (0, lodash_1.get)(configOverride, [
    'localeConfig',
    currentLocale,
  ]);
  const localeConfig = customLocaleConfig || localeConfigs[currentLocale];
  const language = locales_types_1.humanizedDurationLanguages[currentLocale];
  const config = {
    ...generalConfig,
    language,
    ...localeConfig,
    ...configOverride,
  };
  return (0, humanize_duration_1.default)(time, config);
};
//# sourceMappingURL=humanizeDurationByLocale.js.map
