// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { map, get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { NEWS_POLL_INTERVAL } from '../config/timingConfig';
import News from '../domains/News';
import type { GetNewsResponse, NewsItem } from '../api/news/types';

const { isTest } = global.environment;

export default class NewsFeedStore extends Store {
  @observable rawNews: Array<NewsItem>;
  @observable newsUpdatedAt: ?Date = null;
  // @TODO - just if we don't have a data - show error
  @observable fetchingNewsFailed = false;
  @observable
  getNewsRequest: Request<GetNewsResponse> = new Request(this.api.ada.getNews);

  pollingNewsInterval: ?IntervalID = null;

  setup() {
    // Fetch news on app start
    this.getNews();
    if (!isTest) {
      // Refetch news each 30min
      this.pollingNewsInterval = setInterval(this.getNews, NEWS_POLL_INTERVAL);
    }
  }

  @action getNews = async () => {
    const rawNews = await this.getNewsRequest.execute().promise;
    if (rawNews) {
      runInAction('set news data', () => {
        this.rawNews = get(rawNews, 'items', []);
        this.newsUpdatedAt = get(rawNews, 'updatedAt', null);
      });
    }
  };

  @computed get newsFeedData(): News.NewsCollection {
    const { currentLocale } = this.stores.profile;
    let news = [];
    if (this.getNewsRequest.wasExecuted) {
      // @TODO - check news stored in local storage, compare update date and merge data if is needed
      news = map(this.rawNews, item => ({
        ...item,
        title: item.title[currentLocale],
        content: item.title[currentLocale],
        action: {
          ...item.action,
          label: item.action.label[currentLocale],
          url: get(item, ['action', 'url', currentLocale]),
        },
        read: false, // @TODO - check in LC
      }));
    }
    return new News.NewsCollection(news);
  }
}
