'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.waitFor = void 0;
const waitFor = (conditionFunction) => {
  const poll = (resolve) => {
    if (conditionFunction()) resolve();
    else setTimeout(() => poll(resolve), 400);
  };
  return new Promise(poll);
};
exports.waitFor = waitFor;
//# sourceMappingURL=waitFor.js.map
