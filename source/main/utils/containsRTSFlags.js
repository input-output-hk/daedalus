'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.containsRTSFlags = void 0;
const lodash_1 = require('lodash');
const config_1 = require('../config');
const containsRTSFlags = (flags) =>
  (0, lodash_1.isEqual)(flags, config_1.RTS_FLAGS);
exports.containsRTSFlags = containsRTSFlags;
//# sourceMappingURL=containsRTSFlags.js.map
