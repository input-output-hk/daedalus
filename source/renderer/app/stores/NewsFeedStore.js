// @flow
import { observable, action, runInAction, computed } from 'mobx';
import { map, get, find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  NEWS_POLL_INTERVAL,
  NEWS_POLL_INTERVAL_ON_ERROR,
  NEWS_POLL_INTERVAL_ON_INCIDENT,
} from '../config/timingConfig';
import News, { NewsTypes } from '../domains/News';
import type {
  GetNewsResponse,
  GetReadNewsResponse,
  NewsItem,
  MarkNewsAsReadResponse,
} from '../api/news/types';

const { isTest, isDev } = global.environment;

const AVAILABLE_NEWSFEED_EVENT_ACTIONS = [
  'DOWNLOAD_LOGS',
  'OPEN_DIAGNOSTIC_DIALOG',
];

export default class NewsFeedStore extends Store {
  @observable rawNews: ?Array<NewsItem> = null;
  @observable newsUpdatedAt: ?Date = null;
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
  @observable openedAlert: ?News.News = null;
  @observable fetchLocalNews: boolean = false;

  pollingNewsIntervalId: ?IntervalID = null;
  pollingNewsOnErrorIntervalId: ?IntervalID = null;
  pollingNewsOnIncidentIntervalId: ?IntervalID = null;

  setup() {
    // Fetch news on app start
    this.getNews();
    if (!isTest) {
      // Refetch news every 30 mins
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

      const hasIncident = find(
        rawNews.items,
        news => news.type === NewsTypes.INCIDENT
      );

      // Reset "getNews" fast polling interval if set and set regular polling interval
      if (!isTest && this.pollingNewsOnErrorIntervalId) {
        // Reset fast error interval
        clearInterval(this.pollingNewsOnErrorIntervalId);
        this.pollingNewsOnErrorIntervalId = null;

        if (!hasIncident) {
          // set 30 min time interval if NO incidents
          this.pollingNewsIntervalId = setInterval(
            this.getNews,
            NEWS_POLL_INTERVAL
          );
        }
      }

      // If incident occured, reset regular interval and set faster incident interval
      if (hasIncident && !this.pollingNewsOnIncidentIntervalId) {
        // Clear regular interval if set
        if (this.pollingNewsIntervalId) {
          clearInterval(this.pollingNewsIntervalId);
          this.pollingNewsIntervalId = null;
        }

        // Set 10 min time interval and
        this.pollingNewsOnIncidentIntervalId = setInterval(
          this.getNews,
          NEWS_POLL_INTERVAL_ON_INCIDENT
        );
      }

      // If no incidents and incident poller interval active, reset interval and set regular one
      if (!hasIncident && this.pollingNewsOnIncidentIntervalId) {
        // Clear regulat interval
        if (this.pollingNewsOnIncidentIntervalId) {
          clearInterval(this.pollingNewsOnIncidentIntervalId);
          this.pollingNewsOnIncidentIntervalId = null;
        }

        // Set 30 min time interval
        this.pollingNewsIntervalId = setInterval(
          this.getNews,
          NEWS_POLL_INTERVAL
        );
      }

      this._setFetchingNewsFailed(false);
    } catch (error) {
      // Decrease "getNews" fetching timer in case we got an error and there are no initial news set in store
      if (!isTest && !this.rawNews) {
        // Reset all regular intervals
        if (this.pollingNewsIntervalId) {
          clearInterval(this.pollingNewsIntervalId);
          this.pollingNewsIntervalId = null;
        }
        if (this.pollingNewsOnIncidentIntervalId) {
          clearInterval(this.pollingNewsOnIncidentIntervalId);
          this.pollingNewsOnIncidentIntervalId = null;
        }

        // Set fast ERROR interval
        if (!this.pollingNewsOnErrorIntervalId) {
          this.pollingNewsOnErrorIntervalId = setInterval(
            this.getNews,
            NEWS_POLL_INTERVAL_ON_ERROR
          );
        }
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

  @action markNewsAsRead = async newsId => {
    // Set news timestamp to LC
    await this.markNewsAsReadRequest.execute(newsId);
    // Get all read news to force @computed change
    await this.getReadNewsRequest.execute();
  };

  @action openAlert = (newsId: number) => {
    if (this.getNewsRequest.wasExecuted) {
      const alertToOpen = this.newsFeedData.alerts.all.find(
        newsItem => newsItem.id === newsId
      );
      if (alertToOpen) {
        this.openedAlert = alertToOpen;
      }
    }
  };

  @action closeOpenedAlert = () => {
    this.openedAlert = null;
  };

  @action _setFetchingNewsFailed = (fetchingNewsFailed: boolean) => {
    this.fetchingNewsFailed = fetchingNewsFailed;
  };

  @action proceedNewsAction = (newsItem, e: MouseEvent) => {
    const { url, route, event } = newsItem.action;

    if (url) {
      this.stores.app.openExternalLink(url, e);
    } else if (
      route &&
      newsItem.type !== NewsTypes.INCIDENT &&
      newsItem.type !== NewsTypes.ALERT
    ) {
      this.actions.app.closeNewsFeed.trigger();
      this.actions.router.goToRoute.trigger({ route });
    } else if (event && AVAILABLE_NEWSFEED_EVENT_ACTIONS.includes(event)) {
      switch (event) {
        case 'OPEN_DIAGNOSTIC_DIALOG':
          this.actions.app.openDaedalusDiagnosticsDialog.trigger();
          break;
        case 'DOWNLOAD_LOGS':
          this.actions.app.downloadLogs.trigger();
          break;
        default:
          break;
      }
    }
  };

  @action setFakedNewsfeed = () => {
    if (isDev) {
      if (this.pollingNewsIntervalId) {
        clearInterval(this.pollingNewsIntervalId);
        this.pollingNewsIntervalId = null;
      }
      const rawNews = require('../config/news.dummy.json');
      this.rawNews = get(rawNews, 'items', []);
      this.newsUpdatedAt = get(rawNews, 'updatedAt', null);
    }
  };

  @computed get newsFeedData(): News.NewsCollection {
    const { currentLocale } = this.stores.profile;
    const readNews = this.getReadNewsRequest.result;
    let news = [];

    if (this.getNewsRequest.wasExecuted) {
      news = map(this.rawNews, item => {
        // Match old and new newsfeed JSON format
        const mainIdentificator = item.id || item.date;
        return {
          ...item,
          id: mainIdentificator,
          title: item.title[currentLocale],
          content: item.content[currentLocale],
          action: {
            label: get(item, ['action', 'label', currentLocale]),
            url: get(item, ['action', 'url', currentLocale]),
            route: get(item, ['action', 'route', currentLocale]),
            event: get(item, ['action', 'event', currentLocale]),
          },
          date: get(item, ['publishedAt', currentLocale], item.date),
          read: readNews.includes(mainIdentificator),
        };
      });
    }
    return new News.NewsCollection(news);
  }

  @computed get isLoadingNews() {
    return this.fetchingNewsFailed || !this.rawNews;
  }
}
