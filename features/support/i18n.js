import { defineSupportCode } from 'cucumber';

defineSupportCode(({ Before }) => {
  Before(function () {
    this.intl = async (translationId, translationValues = {}) => {
      const translation = await this.client.execute((id, values) => {
        const IntlProvider = require('react-intl').IntlProvider; // eslint-disable-line
        const locale = daedalus.stores.profile.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider({ locale, messages: messages[locale] }, {});
        return intlProvider.getChildContext().intl.formatMessage({ id }, values);
      }, translationId, translationValues);
      return translation.value;
    };
  });
});
