'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const defaultMessages_json_1 = __importDefault(
  require('./locales/defaultMessages.json')
);
const en_US_json_1 = __importDefault(require('./locales/en-US.json'));
const ja_JP_json_1 = __importDefault(require('./locales/ja-JP.json'));
const whitelist_en_US_json_1 = __importDefault(
  require('./locales/whitelist_en-US.json')
);
const whitelist_ja_JP_json_1 = __importDefault(
  require('./locales/whitelist_ja-JP.json')
);
const translations = {
  defaultMessages: defaultMessages_json_1.default,
  'en-US': en_US_json_1.default,
  'ja-JP': ja_JP_json_1.default,
  'whitelist_en-US': whitelist_en_US_json_1.default,
  'whitelist_ja-JP': whitelist_ja_JP_json_1.default,
};
exports.default = translations;
//# sourceMappingURL=translations.js.map
