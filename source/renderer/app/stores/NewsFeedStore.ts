import { observable, action, runInAction, computed } from 'mobx';
import { map, get, find } from 'lodash';
import Store from './lib/Store';
import Request from './lib/LocalizedRequest';
import {
  NEWS_POLL_INTERVAL,
  NEWS_POLL_INTERVAL_ON_ERROR,
  NEWS_POLL_INTERVAL_ON_INCIDENT,
} from '../config/timingConfig';
import News, { NewsTypes, IncidentColors } from '../domains/News';
import type {
  GetNewsResponse,
  GetReadNewsResponse,
  NewsItem,
  MarkNewsAsReadResponse,
} from '../api/news/types';

const { isTest, version, isDev } = global.environment;
const AVAILABLE_NEWSFEED_EVENT_ACTIONS = [
  'DOWNLOAD_LOGS',
  'OPEN_DIAGNOSTIC_DIALOG',
];
export default class NewsFeedStore extends Store {
  @observable
  rawNews: Array<NewsItem> | null | undefined = null;
  @observable
  newsUpdatedAt: Date | null | undefined = null;
  @observable
  fetchingNewsFailed = false;
  @observable
  getNewsRequest: Request<GetNewsResponse> = new Request(this.api.ada.getNews);
  @observable
  getReadNewsRequest: Request<GetReadNewsResponse> = new Request(
    this.api.localStorage.getReadNews
  );
  @observable
  markNewsAsReadRequest: Request<MarkNewsAsReadResponse> = new Request(
    this.api.localStorage.markNewsAsRead
  );
  @observable
  markNewsAsUnreadRequest: Request<MarkNewsAsReadResponse> = new Request(
    this.api.localStorage.markNewsAsUnread
  );
  @observable
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  openedAlert: News.News | null | undefined = null;
  @observable
  fetchLocalNews = false;
  @observable
  rawNewsJsonQA: GetNewsResponse | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingNewsIntervalId: IntervalID | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingNewsOnErrorIntervalId: IntervalID | null | undefined = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingNewsOnIncidentIntervalId: IntervalID | null | undefined = null;

  setup() {
    // Fetch news on app start
    this.getNews({
      isInit: true,
    });

    if (!isTest) {
      // Refetch news every 30 mins
      this.pollingNewsIntervalId = setInterval(
        this.getNews,
        NEWS_POLL_INTERVAL
      );
    }
  }

  @action
  getNews = async (params?: { isInit: boolean }) => {
    let rawNews;

    try {
      if (this.rawNewsJsonQA && isDev) {
        rawNews = this.rawNewsJsonQA;
      } else {
        rawNews = await this.getNewsRequest.execute().promise;
      }

      const hasIncident = find(
        rawNews.items,
        (news) => news.type === NewsTypes.INCIDENT
      );

      // Check for "Alerts" with repeatable state and set as unread
      if (params && params.isInit && rawNews) {
        const repeatableNews = find(
          rawNews.items,
          (news) => news.type === NewsTypes.ALERT && news.repeatOnStartup
        );

        if (repeatableNews) {
          const mainIdentificator = repeatableNews.id || repeatableNews.date;
          // Mark Alert as unread in LC if "repeatOnStartup" parameter set
          // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
          await this.markNewsAsUnreadRequest.execute(mainIdentificator);
          // Get all read news to force @computed change
          // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
          await this.getReadNewsRequest.execute();
        }
      }

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

      // If incident occurred, reset regular interval and set faster incident interval
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

    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getReadNewsRequest.execute();

    if (rawNews) {
      runInAction('set news data', () => {
        this.rawNews = get(rawNews, 'items', []);
        this.newsUpdatedAt = get(rawNews, 'updatedAt', null);
      });
    }
  };
  @action
  markNewsAsRead = async (newsId: number[]) => {
    // Set news timestamp to LC
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.markNewsAsReadRequest.execute(newsId);
    // Get all read news to force @computed change
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getReadNewsRequest.execute();
  };
  @action
  openAlert = (newsId: number) => {
    if (this.getNewsRequest.wasExecuted) {
      const alertToOpen = this.newsFeedData.alerts.all.find(
        (newsItem) => newsItem.id === newsId
      );

      if (alertToOpen) {
        this.openedAlert = alertToOpen;
      }
    }
  };
  @action
  closeOpenedAlert = () => {
    this.openedAlert = null;
  };
  @action
  _setFetchingNewsFailed = (fetchingNewsFailed: boolean) => {
    this.fetchingNewsFailed = fetchingNewsFailed;
  };
  @action
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  proceedNewsAction = (newsItem: News.News, e: MouseEvent) => {
    const { url, route, event } = newsItem.action;

    if (url) {
      this.stores.app.openExternalLink(url, e);
    } else if (
      route &&
      newsItem.type !== NewsTypes.INCIDENT &&
      newsItem.type !== NewsTypes.ALERT
    ) {
      // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
      this.actions.app.closeNewsFeed.trigger();
      this.actions.router.goToRoute.trigger({
        route,
      });
    } else if (event && AVAILABLE_NEWSFEED_EVENT_ACTIONS.includes(event)) {
      switch (event) {
        case 'OPEN_DIAGNOSTIC_DIALOG':
          // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
          this.actions.app.openDaedalusDiagnosticsDialog.trigger();
          break;

        case 'DOWNLOAD_LOGS':
          // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
          this.actions.app.downloadLogs.trigger();
          break;

        default:
          break;
      }
    }
  };
  @action
  setFakedNewsfeed = (params: {
    isAutomaticUpdateTest: boolean | null | undefined;
    appVersion?: string;
  }) => {
    if (isDev) {
      const { isAutomaticUpdateTest, appVersion } = params;
      // Fake appVersion for news ONLY so we can check multiple cases
      global.environment.version = appVersion || version;

      if (this.pollingNewsIntervalId) {
        clearInterval(this.pollingNewsIntervalId);
        this.pollingNewsIntervalId = null;
      }

      let rawNewsJsonQA;

      if (isAutomaticUpdateTest) {
        rawNewsJsonQA = require('../config/newsfeed-files/news-automatic-update.dummy.json');
      } else {
        rawNewsJsonQA = require('../config/news.dummy.json');
      }

      this.rawNewsJsonQA = rawNewsJsonQA;
      this.getNews({
        isInit: true,
      });
    }
  };

  @computed
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  get newsFeedData(): News.NewsCollection {
    const { currentLocale } = this.stores.profile;
    const readNews = this.getReadNewsRequest.result;
    let news = [];

    if (this.getNewsRequest.wasExecuted || (this.rawNewsJsonQA && isDev)) {
      news = map(this.rawNews, (item) => {
        // Match old and new newsfeed JSON format
        // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'NewsItem'.
        const mainIdentificator = item.id || item.date;
        let newsfeedItem = {
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
          // @ts-ignore ts-migrate(2339) FIXME: Property 'includes' does not exist on type 'GetRea... Remove this comment to see the full error message
          read: readNews.includes(mainIdentificator),
        };

        // Exclude "color" parameter from news that are not incidents
        if (item.type === NewsTypes.INCIDENT) {
          newsfeedItem = {
            ...newsfeedItem,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ color: any; id: any; title: string; conten... Remove this comment to see the full error message
            color: get(item, 'color', IncidentColors.RED),
          };
        }

        // Exclude "repeatOnStartup" parameter from news that are not alerts
        if (item.type === NewsTypes.ALERT) {
          newsfeedItem = {
            ...newsfeedItem,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ repeatOnStartup: any; id: any; title: stri... Remove this comment to see the full error message
            repeatOnStartup: get(item, 'repeatOnStartup', false),
          };
        }

        return newsfeedItem;
      });
    }

    return new News.NewsCollection(news);
  }

  @computed
  get isLoadingNews() {
    return this.fetchingNewsFailed || !this.rawNews;
  }
}
