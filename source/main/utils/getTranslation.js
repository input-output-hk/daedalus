'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.getTranslation = void 0;
const getTranslation = (translations, id) => (key) =>
  translations[`${id}.${key}`];
exports.getTranslation = getTranslation;
//# sourceMappingURL=getTranslation.js.map
