'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.submitOnEnter = void 0;
const submitOnEnter = (callback, event) => {
  if (event.target instanceof HTMLInputElement && event.key === 'Enter')
    callback();
};
exports.submitOnEnter = submitOnEnter;
//# sourceMappingURL=form.js.map
