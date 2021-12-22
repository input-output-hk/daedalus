import { IntlProvider } from 'react-intl';
// context that can be used outside React Component
// e.g. intl.formatMessage() directly in pdf generator
export const i18nContext = (locale) => {
  const messages = require(`../i18n/locales/${locale}.json`);

  const intlProvider = new IntlProvider(
    {
      locale,
      messages,
    },
    {}
  );
  const { intl } = intlProvider.getChildContext();
  return intl;
};
