'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.WalletRewardsOpenCsvError = exports.WalletPaperWalletOpenPdfError = exports.WalletSupportRequestLogsCompressError = exports.FieldRequiredError = exports.InvalidEmailError = exports.InvalidMnemonicError = void 0;
const LocalizableError_1 = __importDefault(require('./LocalizableError'));
class InvalidMnemonicError extends LocalizableError_1.default {
  constructor() {
    super({
      id: 'global.errors.invalidMnemonic',
      defaultMessage: '!!!Invalid phrase entered, please check.',
    });
  }
}
exports.InvalidMnemonicError = InvalidMnemonicError;
class InvalidEmailError extends LocalizableError_1.default {
  constructor() {
    super({
      id: 'global.errors.invalidEmail',
      defaultMessage: '!!!Invalid email entered, please check.',
    });
  }
}
exports.InvalidEmailError = InvalidEmailError;
class FieldRequiredError extends LocalizableError_1.default {
  constructor() {
    super({
      id: 'global.errors.fieldIsRequired',
      defaultMessage: '!!!This field is required.',
    });
  }
}
exports.FieldRequiredError = FieldRequiredError;
class WalletSupportRequestLogsCompressError extends LocalizableError_1.default {
  constructor() {
    super({
      id: 'global.errors.WalletSupportRequestLogsCompressError',
      defaultMessage: '!!!Compressing logs failed, please try again.',
    });
  }
}
exports.WalletSupportRequestLogsCompressError = WalletSupportRequestLogsCompressError;
class WalletPaperWalletOpenPdfError extends LocalizableError_1.default {
  constructor() {
    super({
      id: 'global.errors.paperWalletOpenPdfError',
      defaultMessage:
        '!!!The file you are trying to replace is open. Please close it and try again.',
    });
  }
}
exports.WalletPaperWalletOpenPdfError = WalletPaperWalletOpenPdfError;
class WalletRewardsOpenCsvError extends LocalizableError_1.default {
  constructor() {
    super({
      id: 'global.errors.rewardsOpenCsvError',
      defaultMessage:
        '!!!The file you are trying to replace is open. Please close it and try again.',
    });
  }
}
exports.WalletRewardsOpenCsvError = WalletRewardsOpenCsvError;
//# sourceMappingURL=errors.js.map
