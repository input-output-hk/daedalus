'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.detectSystemLocale = void 0;
const electron_1 = require('electron');
const logging_1 = require('./logging');
const locales_types_1 = require('../../common/types/locales.types');
const detectSystemLocale = () => {
  const systemLocale = electron_1.app.getLocale();
  logging_1.logger.info('Detected system locale', {
    systemLocale,
  });
  if (systemLocale === 'ja') {
    return locales_types_1.LOCALES.japanese;
  }
  return locales_types_1.LOCALES.english;
};
exports.detectSystemLocale = detectSystemLocale;
//# sourceMappingURL=detectSystemLocale.js.map
