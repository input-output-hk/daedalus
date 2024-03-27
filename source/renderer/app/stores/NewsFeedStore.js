'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
var _a, _b;
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const Store_1 = __importDefault(require('./lib/Store'));
const LocalizedRequest_1 = __importDefault(require('./lib/LocalizedRequest'));
const timingConfig_1 = require('../config/timingConfig');
const News_1 = __importStar(require('../domains/News'));
const { isTest, version, isDev } = global.environment;
const AVAILABLE_NEWSFEED_EVENT_ACTIONS = [
  'DOWNLOAD_LOGS',
  'OPEN_DIAGNOSTIC_DIALOG',
];
class NewsFeedStore extends Store_1.default {
  rawNews = null;
  newsUpdatedAt = null;
  fetchingNewsFailed = false;
  getNewsRequest = new LocalizedRequest_1.default(this.api.ada.getNews);
  getReadNewsRequest = new LocalizedRequest_1.default(
    this.api.localStorage.getReadNews
  );
  markNewsAsReadRequest = new LocalizedRequest_1.default(
    this.api.localStorage.markNewsAsRead
  );
  markNewsAsUnreadRequest = new LocalizedRequest_1.default(
    this.api.localStorage.markNewsAsUnread
  );
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  openedAlert = null;
  fetchLocalNews = false;
  rawNewsJsonQA = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingNewsIntervalId = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingNewsOnErrorIntervalId = null;
  // @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'IntervalID'.
  pollingNewsOnIncidentIntervalId = null;
  setup() {
    // Fetch news on app start
    this.getNews({
      isInit: true,
    });
    if (!isTest) {
      // Refetch news every 30 mins
      this.pollingNewsIntervalId = setInterval(
        this.getNews,
        timingConfig_1.NEWS_POLL_INTERVAL
      );
    }
  }
  getNews = async (params) => {
    let rawNews;
    try {
      if (this.rawNewsJsonQA && isDev) {
        rawNews = this.rawNewsJsonQA;
      } else {
        rawNews = await this.getNewsRequest.execute().promise;
      }
      const hasIncident = (0, lodash_1.find)(
        rawNews.items,
        (news) => news.type === News_1.NewsTypes.INCIDENT
      );
      // Check for "Alerts" with repeatable state and set as unread
      if (params && params.isInit && rawNews) {
        const repeatableNews = (0, lodash_1.find)(
          rawNews.items,
          (news) => news.type === News_1.NewsTypes.ALERT && news.repeatOnStartup
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
            timingConfig_1.NEWS_POLL_INTERVAL
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
          timingConfig_1.NEWS_POLL_INTERVAL_ON_INCIDENT
        );
      }
      // If no incidents and incident poller interval active, reset interval and set regular one
      if (!hasIncident && this.pollingNewsOnIncidentIntervalId) {
        // Clear regular interval
        if (this.pollingNewsOnIncidentIntervalId) {
          clearInterval(this.pollingNewsOnIncidentIntervalId);
          this.pollingNewsOnIncidentIntervalId = null;
        }
        // Set 30 min time interval
        this.pollingNewsIntervalId = setInterval(
          this.getNews,
          timingConfig_1.NEWS_POLL_INTERVAL
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
            timingConfig_1.NEWS_POLL_INTERVAL_ON_ERROR
          );
        }
      }
      this._setFetchingNewsFailed(true);
    }
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getReadNewsRequest.execute();
    if (rawNews) {
      (0, mobx_1.runInAction)('set news data', () => {
        this.rawNews = (0, lodash_1.get)(rawNews, 'items', []);
        this.newsUpdatedAt = (0, lodash_1.get)(rawNews, 'updatedAt', null);
      });
    }
  };
  markNewsAsRead = async (newsId) => {
    // Set news timestamp to LC
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.markNewsAsReadRequest.execute(newsId);
    // Get all read news to force @computed change
    // @ts-ignore ts-migrate(1320) FIXME: Type of 'await' operand must either be a valid pro... Remove this comment to see the full error message
    await this.getReadNewsRequest.execute();
  };
  openAlert = (newsId) => {
    if (this.getNewsRequest.wasExecuted) {
      const alertToOpen = this.newsFeedData.alerts.all.find(
        (newsItem) => newsItem.id === newsId
      );
      if (alertToOpen) {
        this.openedAlert = alertToOpen;
      }
    }
  };
  closeOpenedAlert = () => {
    this.openedAlert = null;
  };
  _setFetchingNewsFailed = (fetchingNewsFailed) => {
    this.fetchingNewsFailed = fetchingNewsFailed;
  };
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
  proceedNewsAction = (newsItem, e) => {
    const { url, route, event } = newsItem.action;
    if (url) {
      this.stores.app.openExternalLink(url, e);
    } else if (
      route &&
      newsItem.type !== News_1.NewsTypes.INCIDENT &&
      newsItem.type !== News_1.NewsTypes.ALERT
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
  setFakedNewsfeed = (params) => {
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
  get newsFeedData() {
    const { currentLocale } = this.stores.profile;
    const readNews = this.getReadNewsRequest.result;
    let news = [];
    if (this.getNewsRequest.wasExecuted || (this.rawNewsJsonQA && isDev)) {
      news = (0, lodash_1.map)(this.rawNews, (item) => {
        // Match old and new newsfeed JSON format
        // @ts-ignore ts-migrate(2339) FIXME: Property 'id' does not exist on type 'NewsItem'.
        const mainIdentificator = item.id || item.date;
        let newsfeedItem = {
          ...item,
          id: mainIdentificator,
          title: item.title[currentLocale],
          content: item.content[currentLocale],
          action: {
            label: (0, lodash_1.get)(item, ['action', 'label', currentLocale]),
            url: (0, lodash_1.get)(item, ['action', 'url', currentLocale]),
            route: (0, lodash_1.get)(item, ['action', 'route', currentLocale]),
            event: (0, lodash_1.get)(item, ['action', 'event', currentLocale]),
          },
          date: (0, lodash_1.get)(
            item,
            ['publishedAt', currentLocale],
            item.date
          ),
          // @ts-ignore ts-migrate(2339) FIXME: Property 'includes' does not exist on type 'GetRea... Remove this comment to see the full error message
          read: readNews.includes(mainIdentificator),
        };
        // Exclude "color" parameter from news that are not incidents
        if (item.type === News_1.NewsTypes.INCIDENT) {
          newsfeedItem = {
            ...newsfeedItem,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ color: any; id: any; title: string; conten... Remove this comment to see the full error message
            color: (0, lodash_1.get)(item, 'color', News_1.IncidentColors.RED),
          };
        }
        // Exclude "repeatOnStartup" parameter from news that are not alerts
        if (item.type === News_1.NewsTypes.ALERT) {
          newsfeedItem = {
            ...newsfeedItem,
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ repeatOnStartup: any; id: any; title: stri... Remove this comment to see the full error message
            repeatOnStartup: (0, lodash_1.get)(item, 'repeatOnStartup', false),
          };
        }
        return newsfeedItem;
      });
    }
    return new News_1.default.NewsCollection(news);
  }
  get isLoadingNews() {
    return this.fetchingNewsFailed || !this.rawNews;
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  NewsFeedStore.prototype,
  'rawNews',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Date)],
  NewsFeedStore.prototype,
  'newsUpdatedAt',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'fetchingNewsFailed',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NewsFeedStore.prototype,
  'getNewsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NewsFeedStore.prototype,
  'getReadNewsRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NewsFeedStore.prototype,
  'markNewsAsReadRequest',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', LocalizedRequest_1.default)],
  NewsFeedStore.prototype,
  'markNewsAsUnreadRequest',
  void 0
);
__decorate(
  [
    mobx_1.observable,
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
    __metadata(
      'design:type',
      typeof (_a =
        typeof News_1.default !== 'undefined' && News_1.default.News) ===
        'function'
        ? _a
        : Object
    ),
  ],
  NewsFeedStore.prototype,
  'openedAlert',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'fetchLocalNews',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'rawNewsJsonQA',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'getNews',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'markNewsAsRead',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'openAlert',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'closeOpenedAlert',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  '_setFetchingNewsFailed',
  void 0
);
__decorate(
  [
    mobx_1.action,
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
    __metadata('design:type', Object),
  ],
  NewsFeedStore.prototype,
  'proceedNewsAction',
  void 0
);
__decorate(
  [mobx_1.action, __metadata('design:type', Object)],
  NewsFeedStore.prototype,
  'setFakedNewsfeed',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
    __metadata(
      'design:type',
      typeof (_b =
        typeof News_1.default !== 'undefined' &&
        News_1.default.NewsCollection) === 'function'
        ? _b
        : Object
    ),
    __metadata('design:paramtypes', []),
  ],
  NewsFeedStore.prototype,
  'newsFeedData',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  NewsFeedStore.prototype,
  'isLoadingNews',
  null
);
exports.default = NewsFeedStore;
//# sourceMappingURL=NewsFeedStore.js.map
