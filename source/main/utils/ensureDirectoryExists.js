'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const mkdirp_1 = __importDefault(require('mkdirp'));
const fs_1 = __importDefault(require('fs'));
exports.default = (filepath) => {
  let stats;
  try {
    stats = fs_1.default.lstatSync(filepath);
  } catch (e) {
    try {
      mkdirp_1.default.sync(filepath);
      stats = fs_1.default.lstatSync(filepath);
    } catch (error) {
      process.exit(1);
    }
  }
  if (!stats || !stats.isDirectory()) {
    process.exit(1);
  }
};
//# sourceMappingURL=ensureDirectoryExists.js.map
