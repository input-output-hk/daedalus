'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.i18nContext = void 0;
const react_intl_1 = require('react-intl');
// context that can be used outside React Component
// e.g. intl.formatMessage() directly in pdf generator
const i18nContext = (locale) => {
  const messages = require(`../i18n/locales/${locale}.json`);
  const intlProvider = new react_intl_1.IntlProvider(
    {
      locale,
      messages,
    },
    {}
  );
  const { intl } = intlProvider.getChildContext();
  return intl;
};
exports.i18nContext = i18nContext;
//# sourceMappingURL=i18nContext.js.map
