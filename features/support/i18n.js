export default function () {
  this.Before(function() {
    this.intl = async (translationId, translationValues = {}) => {
      const translation = await this.client.execute(function(id, values) {
        const IntlProvider = require('react-intl').IntlProvider;
        const locale = daedalus.stores.app.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider({ locale, messages: messages[locale] }, {});
        return intlProvider.getChildContext().intl.formatMessage({ id }, values);
      }, translationId, translationValues);
      return translation.value;
    };
  });
}
