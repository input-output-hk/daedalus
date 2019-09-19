// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { map, get } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  NEWS_POLL_INTERVAL,
  NEWS_POLL_INTERVAL_ON_ERROR,
} from '../config/timingConfig';
import News from '../domains/News';
import type {
  GetNewsResponse,
  GetReadNewsResponse,
  NewsItem,
  MarkNewsAsReadResponse,
} from '../api/news/types';

const { isTest } = global.environment;

export default class NewsFeedStore extends Store {
  @observable rawNews: ?Array<NewsItem> = null;
  @observable newsUpdatedAt: ?Date = null;
  // @TODO - just if we don't have a data - show error
  @observable fetchingNewsFailed = false;
  @observable getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  );
  @observable getReadNewsRequest: Request<GetReadNewsResponse> = new Request(
    this.api.localStorage.getReadNews
  );
  @observable
  markNewsAsReadRequest: Request<MarkNewsAsReadResponse> = new Request(
    this.api.localStorage.markNewsAsRead
  );

  pollingNewsIntervalId: ?IntervalID = null;
  pollingNewsOnErrorIntervalId: ?IntervalID = null;

  setup() {
    // Fetch news on app start
    this.getNews();
    if (!isTest) {
      // Refetch news each 30min
      this.pollingNewsIntervalId = setInterval(
        this.getNews,
        NEWS_POLL_INTERVAL
      );
    }
  }

  @action getNews = async () => {
    let rawNews;
    try {
      rawNews = await this.getNewsRequest.execute().promise;
      // Reset "getNews" fast polling interval if set and set again regular polling interval
      if (!isTest && this.pollingNewsOnErrorIntervalId) {
        clearInterval(this.pollingNewsIntervalId);
        this.pollingNewsOnErrorIntervalId = null;
        this.pollingNewsIntervalId = setInterval(
          this.getNews,
          NEWS_POLL_INTERVAL
        );
      }
      this._setFetchingNewsFailed(false);
    } catch (error) {
      // Decrease "getNews" fetching timer in case we got an error and there are no initial news set in store
      if (!isTest && !this.rawNews && this.pollingNewsIntervalId) {
        clearInterval(this.pollingNewsIntervalId);
        this.pollingNewsIntervalId = null;
        this.pollingNewsOnErrorIntervalId = setInterval(
          this.getNews,
          NEWS_POLL_INTERVAL_ON_ERROR
        );
      }
      this._setFetchingNewsFailed(true);
    }

    await this.getReadNewsRequest.execute();

    if (rawNews) {
      runInAction('set news data', () => {
        this.rawNews = get(rawNews, 'items', []);
        this.newsUpdatedAt = get(rawNews, 'updatedAt', null);
      });
    }
  };

  @action markNewsAsRead = async newsTimestamps => {
    // Set news timestamp to LC
    await this.markNewsAsReadRequest.execute(newsTimestamps);
    // GET all read news to force @computed to trigger
    await this.getReadNewsRequest.execute();
  };

  @action _setFetchingNewsFailed = (fetchingNewsFailed: boolean) => {
    this.fetchingNewsFailed = fetchingNewsFailed;
  };

  @computed get newsFeedData(): News.NewsCollection {
    const { currentLocale } = this.stores.profile;
    const readNews = this.getReadNewsRequest.result;
    let news = [];
    if (this.getNewsRequest.wasExecuted) {
      news = map(this.rawNews, item => ({
        ...item,
        title: item.title[currentLocale],
        content: item.content[currentLocale],
        action: {
          ...item.action,
          label: item.action.label[currentLocale],
          url: get(item, ['action', 'url', currentLocale]),
        },
        read: readNews.includes(item.date),
      }));
    }

    return new News.NewsCollection(news);
  }

  @computed get isLoadingNews() {
    return this.fetchingNewsFailed || !this.rawNews;
  }
}
