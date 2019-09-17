// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { map, get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { NEWS_POLL_INTERVAL } from '../config/timingConfig';
import type { GetNewsResponse } from '../api/news/types';

const { isTest } = global.environment;

export default class NewsFeedStore extends Store {
  @observable newsData = null;
  @observable newsUpdatedAt = null;
  // @TODO - jsut if we don't have a data - show error
  @observable fetchingNewsFailed = false;
  @observable
  getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  );

  setup() {
    // Fetch news on app start
    this.getNews();
    if (!isTest) {
      // Refetch news each 30min
      setInterval(this.getNews, NEWS_POLL_INTERVAL);
    }
  }

  @action getNews = async () => {
    const result = await this.getNewsRequest.execute().promise;
    console.debug('>>> execute newsData');
    if (result) {
      console.debug('>>> SET newsData');
      runInAction('set news data', () => {
        this.newsData = result;
      });
    }
  };

  @computed get newsFeedData(): any {
    console.debug('@computed: ', this.newsData);
    const currentLocale = this.stores.profile.currentLocale;
    const newsFeedItems = get(this.newsData, ['result', 'items']);
    const newsCreatedAt = get(this.newsData, ['result', 'createdAt']);

    // @TODO - check news stored in local storage, comppare dates and merge data if is needed

    return map(newsFeedItems, (item) => ({
      ...item,
      title: item.title[currentLocale],
      content: item.title[currentLocale],
      action: {
        ...item.action,
        label: item.action.label[currentLocale],
        url: get(item, ['action', 'url', currentLocale]),
      },
      read: false // @TODO - check in LC
    }));

  }
}