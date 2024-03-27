'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const crypto_1 = require('./crypto');
const PersistentTimeMachine_1 = require('./PersistentTimeMachine');
const timeMachine = new PersistentTimeMachine_1.PersistentTimeMachine();
timeMachine.init();
exports.default = {
  crypto: {
    generateMnemonic: crypto_1.generateMnemonic,
  },
  timeMachine,
};
//# sourceMappingURL=index.js.map
