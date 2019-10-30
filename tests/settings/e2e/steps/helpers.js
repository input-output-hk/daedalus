// @flow
import type { Daedalus, WebdriverClient } from '../../../types';

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';
const DEFAULT_LANGUAGE = 'en-US';
const INITIAL_SETTINGS_FORM = '.InitialSettings_component';
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
    client.execute(value => {
      daedalus.actions.profile.updateUserLocalSetting.trigger({ param: 'locale', value });
    }, language || DEFAULT_LANGUAGE),
};

export const initialSettingsHelpers = {
  waitForVisible: async (
    client: WebdriverClient,
    { isHidden }: { isHidden: boolean } = {}
  ) =>
    client.waitForVisible(INITIAL_SETTINGS_FORM, null, isHidden),
  ensureLanguageIsSelected: async (
    client: WebdriverClient,
    { language }: { language: string } = {}
  ) => {
    await i18nHelpers.setActiveLanguage(client, { language });
    client.execute(
      () => daedalus.actions.profile.finishInitialScreenSettings.trigger()
    );
    await initialSettingsHelpers.waitForVisible(client, { isHidden: true });
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
