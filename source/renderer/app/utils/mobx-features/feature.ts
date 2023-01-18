import pull from 'lodash/pull';
import { Reaction } from './reaction';

export class Feature {
  isRunning = false;
  reactions: Reaction[] = [];

  async start() {
    this.startReactions();
    this.isRunning = true;
  }

  async stop() {
    this.stopReactions();
    this.isRunning = false;
  }

  // REACTIONS
  registerReactions(reactions: Reaction[]) {
    this.reactions.push(...reactions);
  }

  unregisterReactions(reactions: Reaction[]) {
    pull(this.reactions, ...reactions);
  }

  startReactions(reactions: Reaction[] = this.reactions) {
    reactions.forEach((r) => r.start());
  }

  stopReactions(reactions: Reaction[] = this.reactions) {
    reactions.forEach((r) => r.stop());
  }
}
