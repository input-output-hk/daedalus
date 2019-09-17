// @flow
import { observable, action } from 'mobx';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { NEWS_POLL_INTERVAL } from '../config/timingConfig';
import type { GetNewsResponse } from '../api/news/types';

const { isTest } = global.environment;

export default class NewsFeedStore extends Store {
  @observable newsFeed = [];
  @observable newsUpdatedAt = null;
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

  getNews = async () => {
    const result = await this.getNewsRequest.execute().promise;
    if (result) {
      this._setNewsFeed(result);
    }
  };

  @action _setNewsFeed = (newsFeed) => {
    this.newsFeed = newsFeed.items;
    this.newsUpdatedAt = newsFeed.updatedAt;
  };
}