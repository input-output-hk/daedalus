// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { map, get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import { NEWS_POLL_INTERVAL } from '../config/timingConfig';
import News from '../domains/News';
import type {
  GetNewsResponse,
  GetReadNewsResponse,
  NewsItem,
} from '../api/news/types';

const { isTest } = global.environment;

export default class NewsFeedStore extends Store {
  @observable newsItems: Array<NewsItem>;
  @observable newsUpdatedAt: ?Date = null;
  // @TODO - just if we don't have a data - show error
  @observable fetchingNewsFailed = false;
  @observable getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  );

  @observable getReadNewsRequest: Request<GetReadNewsResponse> = new Request(
    this.api.localStorage.getReadNews
  );

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
    await this.getReadNewsRequest.execute();
    const newsData = await this.getNewsRequest.execute().promise;
    if (newsData) {
      runInAction('set news data', () => {
        this.newsItems = get(newsData, 'items', []);
        this.newsUpdatedAt = get(newsData, 'updatedAt', null);
      });
    }
  };

  @computed get newsFeedData(): Array<News> {
    const { currentLocale } = this.stores.profile;
    const readNews = this.getReadNewsRequest.result;
    let newsFeedData = [];
    if (this.getNewsRequest.wasExecuted) {
      // @TODO - check news stored in local storage, compare update date and merge data if is needed
      newsFeedData = map(this.newsItems, item => ({
        ...item,
        title: item.title[currentLocale],
        content: item.title[currentLocale],
        action: {
          ...item.action,
          label: item.action.label[currentLocale],
          url: get(item, ['action', 'url', currentLocale]),
        },
        read: readNews.includes(item.date),
      }));
      /* return new News({

      }) */
    }

    return newsFeedData;
  }
}
