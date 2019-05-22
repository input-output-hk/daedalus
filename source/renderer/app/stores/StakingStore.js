// @flow
import { computed, action } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';

export default class StakingStore extends Store {
  startDateTime: string = '2019-05-26T23:00:51.161Z';
  decentralizationProgress: number = 10;

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @action showCountDown(): boolean {
    return new Date(this.startDateTime).getTime() - new Date().getTime() > 0;
  }
}
