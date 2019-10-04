// @flow
import type { Daedalus, WebdriverClient } from '../../../types';

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';
const DEFAULT_LANGUAGE = 'en-US';
const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';
const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

declare var daedalus: Daedalus;

export const i18nHelpers = {
  formatMessage: async (
    client: WebdriverClient,
    { id, values }: { id: string, values?: Object }
  ) => {
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
  setActiveLanguage: async (
    client: WebdriverClient,
    { language }: { language: string } = {}
  ) =>
    client.execute(locale => {
      daedalus.actions.profile.updateLocale.trigger({ locale });
    }, language || DEFAULT_LANGUAGE),
};

export const languageSelectionHelpers = {
  waitForVisible: async (
    client: WebdriverClient,
    { isHidden }: { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(LANGUAGE_SELECTION_FORM, null, isHidden),
  ensureLanguageIsSelected: async (
    client: WebdriverClient,
    { language }: { language: string } = {}
  ) => {
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
  waitForVisible: async (
    client: WebdriverClient,
    { isHidden } : { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(TERMS_OF_USE_FORM, null, isHidden),
  acceptTerms: async (client: WebdriverClient) => {
    await client.execute(() => {
      daedalus.actions.profile.acceptTermsOfUse.trigger();
    });
    await termsOfUseHelpers.waitForVisible(client, { isHidden: true });
  },
};
