import i18n from './i18n-helpers';
import type { WebdriverClient } from '../../../types';

const DATA_LAYER_MIGRATION_ACCEPTANCE_COMPONENT = '.DataLayerMigrationForm_component';
const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';
const TERMS_OF_USE_FORM = '.TermsOfUseForm_component';

export const languageSelectionHelpers = {
  waitForVisible: async (client, { isHidden } = {}) =>
    client.waitForVisible(LANGUAGE_SELECTION_FORM, null, isHidden),
  ensureLanguageIsSelected: async (client, { language } = {}) => {
    await languageSelectionHelpers.waitForVisible(client);
    await i18n.setActiveLanguage(client, { language });
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