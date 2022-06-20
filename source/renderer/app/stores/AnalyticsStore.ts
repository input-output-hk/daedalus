import { observable, runInAction } from 'mobx';
import Store from './lib/Store';
import { AnalyticsClient, getAnalyticsClient } from '../analytics';

export default class AnalyticsStore extends Store {
  @observable
  analyticsClient: AnalyticsClient;

  setup() {
    this.resetAnalyticsClient();
  }

  resetAnalyticsClient = async (): Promise<void> => {
    const client = await getAnalyticsClient(this.api.localStorage, environment);

    runInAction(() => {
      this.analyticsClient = client;
    });
  };
}
