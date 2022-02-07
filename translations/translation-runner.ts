import manageTranslations from "react-intl-translations-manager";

manageTranslations({
  messagesDirectory: 'translations/messages',
  translationsDirectory: 'source/renderer/app/i18n/locales',
  singleMessagesFile: true,
  languages: ['en-US', 'ja-JP']
});