'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.toJS = void 0;
const toJS = (object) =>
  typeof object === 'object' ? JSON.parse(JSON.stringify(object)) : object;
exports.toJS = toJS;
//# sourceMappingURL=helper.js.map
