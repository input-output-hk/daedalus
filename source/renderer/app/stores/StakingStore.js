// @flow
import Store from './lib/Store';

export default class StakingStore extends Store {
  showCountdown: boolean = false;
  startDateTime: Date = '20191201T000000Z';
}
