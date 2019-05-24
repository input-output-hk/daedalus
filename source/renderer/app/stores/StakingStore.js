// @flow
import { computed, action } from 'mobx';
import Store from './lib/Store';
import { ROUTES } from '../routes-config';

export default class StakingStore extends Store {
  startDateTime: string = '2019-05-26T00:00:00.161Z';
  decentralizationProgress: number = 10;

  setup() {
    const { staking } = this.actions;
    staking.goToStakingInfo.listen(this._goToStakingInfo);
  }

  @computed get currentRoute(): string {
    return this.stores.router.location.pathname;
  }

  @computed get isStakingPage(): boolean {
    return this.currentRoute.indexOf(ROUTES.STAKING.ROOT) > -1;
  }

  @computed
  get isStakingDelegationCountdown(): boolean {
    return this.currentRoute === ROUTES.STAKING.DELEGATION_COUNTDOWN;
  }

  @action showCountdown(): boolean {
    return new Date(this.startDateTime).getTime() - new Date().getTime() > 0;
  }

  _goToStakingInfo = () => {
    this.actions.router.goToRoute.trigger({
      route: ROUTES.STAKING.INFO,
    });
  };
}
