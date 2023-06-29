import { generateMnemonic } from './crypto';
import { PersistentTimeMachine } from './PersistentTimeMachine';

const timeMachine = new PersistentTimeMachine();
timeMachine.init();

export default {
  crypto: {
    generateMnemonic,
  },
  timeMachine,
};
