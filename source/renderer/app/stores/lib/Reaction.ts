import { autorun } from 'mobx';

export default class Reaction {
  reaction: () => void;
  hasBeenStarted: boolean;
  dispose: () => void;

  constructor(reaction: () => void) {
    this.reaction = reaction;
    this.hasBeenStarted = false;
  }

  start() {
    this.dispose = autorun(() => this.reaction());
    this.hasBeenStarted = true;
  }

  stop() {
    if (this.hasBeenStarted) this.dispose();
  }
}
