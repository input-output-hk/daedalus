"use strict";
var __importDefault = (this && this.__importDefault) || function (mod) {
    return (mod && mod.__esModule) ? mod : { "default": mod };
};
Object.defineProperty(exports, "__esModule", { value: true });
const react_intl_translations_manager_1 = __importDefault(require("react-intl-translations-manager"));
const messages_json_1 = __importDefault(require("./messages.json"));
(0, react_intl_translations_manager_1.default)({
    messagesDirectory: 'translations/messages',
    translationsDirectory: 'source/renderer/app/i18n/locales',
    singleMessagesFile: true,
    languages: ['en-US', 'ja-JP'],
    overrideCoreMethods: {
        // Override to use our extracted format.js messages
        provideExtractedMessages: () => messages_json_1.default,
    }
});
//# sourceMappingURL=translation-runner.js.map