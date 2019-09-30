import i18n from './i18n-helpers';

const LANGUAGE_SELECTION_FORM = '.LanguageSelectionForm_component';

const languageSelection = {
  waitForVisible: async (client, { isHidden } = {}) =>
    client.waitForVisible(LANGUAGE_SELECTION_FORM, null, isHidden),
  ensureLanguageIsSelected: async (client, { language } = {}) => {
    await i18n.setActiveLanguage(client, { language });
    await languageSelection.waitForVisible(client, { isHidden: true });
  },
};

export default languageSelection;
