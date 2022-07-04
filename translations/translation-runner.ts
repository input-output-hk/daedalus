import manageTranslations from "react-intl-translations-manager";
import messages from './messages.json';

manageTranslations({
  messagesDirectory: 'translations/messages',
  translationsDirectory: 'source/renderer/app/i18n/locales',
  singleMessagesFile: true,
  languages: ['en-US', 'ja-JP'],
  overrideCoreMethods: {
    // Override to use our extracted format.js messages
    provideExtractedMessages: () => messages,
  }
});
