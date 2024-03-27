'use strict';
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
Object.defineProperty(exports, '__esModule', { value: true });
exports.IncidentColors = exports.NewsTypes = void 0;
const mobx_1 = require('mobx');
const lodash_1 = require('lodash');
const semver_1 = __importDefault(require('semver'));
exports.NewsTypes = {
  INCIDENT: 'incident',
  ALERT: 'alert',
  ANNOUNCEMENT: 'announcement',
  INFO: 'info',
  UPDATE: 'software-update',
};
exports.IncidentColors = {
  RED: 'red',
  THEME_DEFAULT: 'theme-default',
  GREY: 'grey',
};
class News {
  id;
  title;
  content;
  target;
  action;
  date;
  type;
  read;
  color;
  repeatOnStartup;
  constructor(data) {
    Object.assign(this, data);
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  News.prototype,
  'id',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  News.prototype,
  'title',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  News.prototype,
  'content',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  News.prototype,
  'target',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  News.prototype,
  'action',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  News.prototype,
  'date',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  News.prototype,
  'type',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  News.prototype,
  'read',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  News.prototype,
  'color',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  News.prototype,
  'repeatOnStartup',
  void 0
);
class NewsCollection {
  all = [];
  allWithAppUpdates = [];
  constructor(data) {
    const { version, platform } = global.environment;
    // Filter news by platform and versions
    const filteredNewsWithAppUpdates = (0, lodash_1.filter)(
      data,
      (newsItem) => {
        const availableTargetVersionRange = (0, lodash_1.get)(
          newsItem,
          ['target', 'daedalusVersion'],
          ''
        );
        const targetPlatforms = (0, lodash_1.get)(newsItem, [
          'target',
          'platforms',
        ]);
        const isAppUpdateItem = newsItem.type === exports.NewsTypes.UPDATE;
        const hasValidItemLabelDeclaration =
          isAppUpdateItem || (!isAppUpdateItem && newsItem.action.label);
        return (
          (!availableTargetVersionRange ||
            (availableTargetVersionRange &&
              semver_1.default.satisfies(version, availableTargetVersionRange, {
                includePrerelease: true,
              }))) &&
          (platform === 'browser' ||
            (0, lodash_1.includes)(targetPlatforms, platform)) &&
          newsItem.id &&
          newsItem.title &&
          newsItem.content &&
          hasValidItemLabelDeclaration &&
          newsItem.date
        );
      }
    );
    const filteredNewsWithoutAppUpdates = (0, lodash_1.filter)(
      filteredNewsWithAppUpdates,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'type' does not exist on type 'number | N... Remove this comment to see the full error message
      (newsItem) => newsItem.type !== exports.NewsTypes.UPDATE
    );
    const orderedNewsWithoutAppUpdates = (0, lodash_1.orderBy)(
      filteredNewsWithoutAppUpdates,
      'date',
      'desc'
    );
    const orderedNewsWithAppUpdates = (0, lodash_1.orderBy)(
      filteredNewsWithAppUpdates,
      'date',
      'desc'
    );
    (0, mobx_1.runInAction)(() => {
      // @ts-ignore ts-migrate(2322) FIXME: Type '(number | News | (() => string) | { (...item... Remove this comment to see the full error message
      this.all = orderedNewsWithoutAppUpdates;
      // @ts-ignore ts-migrate(2322) FIXME: Type '(number | News | (() => string) | { (...item... Remove this comment to see the full error message
      this.allWithAppUpdates = orderedNewsWithAppUpdates;
    });
  }
  get incident() {
    const incidents = (0, lodash_1.filter)(
      this.all,
      (item) => item.type === exports.NewsTypes.INCIDENT
    );
    const lastIncidentIndex =
      incidents.length > 0 ? incidents.length - 1 : null;
    if (lastIncidentIndex !== null) {
      return incidents[lastIncidentIndex];
    }
    return null;
  }
  get alerts() {
    const alerts = (0, lodash_1.filter)(
      this.all,
      (item) => item.type === exports.NewsTypes.ALERT
    );
    // Order alerts from newest to oldest
    const orderedAlerts = (0, lodash_1.orderBy)(alerts, 'date', 'asc');
    const obj = new NewsCollection(orderedAlerts);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }
  get announcements() {
    const announcements = (0, lodash_1.filter)(
      this.all,
      (item) => item.type === exports.NewsTypes.ANNOUNCEMENT && !item.read
    );
    const obj = new NewsCollection(announcements);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }
  get infos() {
    const infos = (0, lodash_1.filter)(
      this.all,
      (item) => item.type === exports.NewsTypes.INFO && !item.read
    );
    const obj = new NewsCollection(infos);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }
  get unread() {
    const unread = (0, lodash_1.filter)(this.all, (item) => !item.read);
    // Order unread from newest to oldest
    return (0, lodash_1.orderBy)(unread, 'date', 'asc');
  }
  get read() {
    const read = (0, lodash_1.filter)(this.all, (item) => item.read);
    // Order read from newest to oldest
    return (0, lodash_1.orderBy)(read, 'date', 'asc');
  }
  get update() {
    return this.allWithAppUpdates.filter(
      (item) => item.type === exports.NewsTypes.UPDATE
    )[0];
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  NewsCollection.prototype,
  'all',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  NewsCollection.prototype,
  'allWithAppUpdates',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', News),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'incident',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'alerts',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'announcements',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'infos',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'unread',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Array),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'read',
  null
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', News),
    __metadata('design:paramtypes', []),
  ],
  NewsCollection.prototype,
  'update',
  null
);
exports.default = {
  News,
  NewsCollection,
};
//# sourceMappingURL=News.js.map
