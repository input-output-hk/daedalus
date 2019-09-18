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
  @observable rawNews: Array<NewsItem>;
  @observable newsUpdatedAt: ?Date = null;
  // @TODO - just if we don't have a data - show error
  @observable fetchingNewsFailed = false;
  @observable getNewsRequest: Request<GetNewsResponse> = new Request(
    this.api.ada.getNews
  );
  @observable getReadNewsRequest: Request<GetReadNewsResponse> = new Request(
    this.api.localStorage.getReadNews
  );
  @observable markNewsAsReadRequest: Request<MarkNewsAsReadResponse> = new Request(
    this.api.localStorage.markNewsAsRead
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
    const rawNews = await this.getNewsRequest.execute().promise;
    if (rawNews) {
      runInAction('set news data', () => {
        this.rawNews = get(rawNews, 'items', []);
        this.newsUpdatedAt = get(rawNews, 'updatedAt', null);
      });
    }
  };

  // @action markNewsAsRead = (newsTimestamps) => {
  //   const readNews = this.getReadNewsRequest.result;
  //   console.debug('readNews OLD', readNews);
  //   const readNews2 = this.getReadNewsRequest.result;
  //   console.debug('readNews NEW', readNews2);
  // }

  @computed get newsFeedData(): News.NewsCollection {
    console.debug('RUN COMPUTED');
    const { currentLocale } = this.stores.profile;
    const readNews = this.getReadNewsRequest.result;
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
        // read: readNews.includes(item.date),
        read: false,
      }));
    }

    console.debug('START: ', news);
    return new News.NewsCollection(news);
  }
}
