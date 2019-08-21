import type { WebdriverClient } from '../../../types';

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';
const DEFAULT_LANGUAGE = 'en-US';
const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';
const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

export const i18nHelpers = {
  formatMessage: async (client, { id, values }) => {
    const translation = await client.execute(
      (translationId, translationValues) => {
        const IntlProvider = require('react-intl').IntlProvider; // eslint-disable-line
        const locale = daedalus.stores.profile.currentLocale;
        const messages = daedalus.translations;
        const intlProvider = new IntlProvider(
          { locale, messages: messages[locale] },
          {}
        );
        return intlProvider
          .getChildContext()
          .intl.formatMessage({ id: translationId }, translationValues);
      },
      id,
      values || {}
    );
    return translation.value;
  },
  setActiveLanguage: async (client, { language } = {}) =>
    client.execute(locale => {
      daedalus.actions.profile.updateLocale.trigger({ locale });
    }, language || DEFAULT_LANGUAGE),
};

export const languageSelectionHelpers = {
  waitForVisible: async (client, { isHidden } = {}) =>
    client.waitForVisible(LANGUAGE_SELECTION_FORM, null, isHidden),
  ensureLanguageIsSelected: async (client, { language } = {}) => {
    await languageSelectionHelpers.waitForVisible(client);
    await i18nHelpers.setActiveLanguage(client, { language });
    await languageSelectionHelpers.waitForVisible(client, { isHidden: true });
  },
};

export const migrationHelpers = {
  waitForVisible: async (
    client: WebdriverClient,
    { isHidden } : { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(
      DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT,
      null,
      isHidden
    ),
  acceptMigration: async (client: WebdriverClient) => {
    await client.execute(() => {
      daedalus.actions.profile.acceptDataLayerMigration.trigger();
    });
    await migrationHelpers.waitForVisible(client, { isHidden: true });
  },
};

export const termsOfUseHelpers = {
  waitForVisible: async (client, { isHidden } = {}) =>
    client.waitForVisible(TERMS_OF_USE_FORM, null, isHidden),
  acceptTerms: async client => {
    await termsOfUseHelpers.waitForVisible(client);
    await client.execute(() => {
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
    await termsOfUseHelpers.waitForVisible(client, { isHidden: true });
  },
};