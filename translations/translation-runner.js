const manageTranslations = require('react-intl-translations-manager').default;

manageTranslations({
  messagesDirectory: 'translations/messages',
  translationsDirectory: 'source/renderer/app/i18n/locales',
  singleMessagesFile: true,
  languages: ['en-US', 'hr-HR', 'de-DE', 'zh-CN', 'ko-KR', 'ja-JP']
});
