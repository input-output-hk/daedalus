const IntlProvider = require('react-intl').IntlProvider;

export default function () {
  this.Before(() => {
    this.intl = async (translationId, translationValues = {}) => {
      const translation = await this.client.execute((id, values) => {
        const locale = daedalus.stores.app.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider({ locale, messages: messages[locale] }, {});
        return intlProvider.getChildContext().intl.formatMessage({ id }, values);
      }, translationId, translationValues);
      return translation.value;
    };
  });
}
