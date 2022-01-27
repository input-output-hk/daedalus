import { autorun } from 'mobx';

type Fn = () => void;
export class Reaction {
  reaction: Fn;
  isRunning = false;
  dispose: Fn | null = null;

  constructor(reaction: Fn) {
    this.reaction = reaction;
  }

  start() {
    if (!this.isRunning) {
      this.dispose = autorun(this.reaction);
      this.isRunning = true;
    }
  }

  stop() {
    if (this.isRunning && this.dispose) {
      this.dispose();
      this.isRunning = false;
    }
  }
}
export const createReactions = (reactions: Array<Fn>): Array<Reaction> =>
  reactions.map((r) => new Reaction(r));
