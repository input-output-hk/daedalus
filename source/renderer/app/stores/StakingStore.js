// @flow
import { computed } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';

export default class StakingStore extends Store {
  showCountdown: boolean = false;
  startDateTime: string = '20191201T000000Z';

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isTakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }
}
