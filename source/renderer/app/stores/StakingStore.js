// @flow
import { computed } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';

export default class StakingStore extends Store {
  startDateTime: string = '2019-12-01T10:50:10.161Z';
  decentralizationProgress: number = 10;

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @computed get showCountDown(): boolean {
    return new Date(this.startDateTime).getTime() - new Date().getTime() > 0;
  }
}
