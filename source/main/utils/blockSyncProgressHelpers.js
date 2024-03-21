'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.isItFreshLog = void 0;
const moment_1 = __importDefault(require('moment'));
function isItFreshLog(applicationStartDate, line) {
  const [, logDate] =
    line.match(/\[(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2}\.\d+) UTC]/) || [];
  if (!logDate) {
    return false;
  }
  return applicationStartDate.isBefore(moment_1.default.utc(logDate));
}
exports.isItFreshLog = isItFreshLog;
//# sourceMappingURL=blockSyncProgressHelpers.js.map
