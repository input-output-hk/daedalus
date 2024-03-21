'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.TlsCertificateNotValidError = void 0;
const es6_error_1 = __importDefault(require('es6-error'));
class TlsCertificateNotValidError extends es6_error_1.default {
  static API_ERROR = 'CERT_NOT_YET_VALID';
}
exports.TlsCertificateNotValidError = TlsCertificateNotValidError;
//# sourceMappingURL=errors.js.map
