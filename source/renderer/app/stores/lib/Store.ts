import Reaction from './Reaction';
import type { ActionsMap } from '../../actions/index';
import type { StoresMap } from '../index';
import type { Api } from '../../api/index';
import type { Environment } from '../../../../common/types/environment.types';
import { AnalyticsTracker } from '../../analytics';

export default class Store {
  stores: StoresMap;
  environment: Environment = global.environment;
  _reactions: Array<Reaction> = [];

  constructor(
    protected api: Api,
    protected actions: ActionsMap,
    protected analytics: AnalyticsTracker
  ) {}

  registerReactions(reactions: Array<(...args: Array<any>) => any>) {
    reactions.forEach((reaction) =>
      this._reactions.push(new Reaction(reaction))
    );
  }

  configure(stores: StoresMap) {
    this.stores = stores;
  }

  setup() {}

  initialize() {
    this.setup();

    this._reactions.forEach((reaction) => reaction.start());
  }

  teardown() {
    this._reactions.forEach((reaction) => reaction.stop());
  }
}
