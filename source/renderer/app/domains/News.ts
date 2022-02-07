import { observable, computed, runInAction } from 'mobx';
import { get, filter, orderBy, includes } from 'lodash';
import semver from 'semver';
import type { NewsTarget, NewsType } from '../api/news/types';

export type NewsAction = {
  label: string;
  url?: string;
  route?: string;
  event?: string;
};
export type IncidentColor = 'red' | 'theme-default' | 'grey';
export const NewsTypes: {
  INCIDENT: NewsType;
  ALERT: NewsType;
  ANNOUNCEMENT: NewsType;
  INFO: NewsType;
  UPDATE: NewsType;
} = {
  INCIDENT: 'incident',
  ALERT: 'alert',
  ANNOUNCEMENT: 'announcement',
  INFO: 'info',
  UPDATE: 'software-update',
};
export const IncidentColors: {
  RED: IncidentColor;
  THEME_DEFAULT: IncidentColor;
  GREY: IncidentColor;
} = {
  RED: 'red',
  THEME_DEFAULT: 'theme-default',
  GREY: 'grey',
};
export type NewsTypesStateType = {
  all: Array<News>;
  unread: Array<News>;
  read: Array<News>;
};

class News {
  @observable
  id: number;
  @observable
  title: string;
  @observable
  content: string;
  @observable
  target: NewsTarget;
  @observable
  action: NewsAction;
  @observable
  date: number;
  @observable
  type: NewsType;
  @observable
  read: boolean;
  @observable
  color: IncidentColor | null | undefined;
  @observable
  repeatOnStartup: boolean | null | undefined;

  constructor(data: {
    id: number;
    title: string;
    content: string;
    target: NewsTarget;
    action: NewsAction;
    date: number;
    type: NewsType;
    read: boolean;
    color?: IncidentColor | null | undefined;
    repeatOnStartup?: boolean | null | undefined;
  }) {
    Object.assign(this, data);
  }
}

class NewsCollection {
  @observable
  all: Array<News> = [];
  @observable
  allWithAppUpdates: Array<News> = [];

  constructor(data: Array<News>) {
    const { version, platform } = global.environment;
    // Filter news by platform and versions
    const filteredNewsWithAppUpdates = filter(data, (newsItem) => {
      const availableTargetVersionRange = get(
        newsItem,
        ['target', 'daedalusVersion'],
        ''
      );
      const targetPlatforms = get(newsItem, ['target', 'platforms']);
      const isAppUpdateItem = newsItem.type === NewsTypes.UPDATE;
      const hasValidItemLabelDeclaration =
        isAppUpdateItem || (!isAppUpdateItem && newsItem.action.label);
      return (
        (!availableTargetVersionRange ||
          (availableTargetVersionRange &&
            semver.satisfies(version, availableTargetVersionRange, {
              includePrerelease: true,
            }))) &&
        (platform === 'browser' || includes(targetPlatforms, platform)) &&
        newsItem.id &&
        newsItem.title &&
        newsItem.content &&
        hasValidItemLabelDeclaration &&
        newsItem.date
      );
    });
    const filteredNewsWithoutAppUpdates = filter(
      filteredNewsWithAppUpdates,
      // @ts-ignore ts-migrate(2339) FIXME: Property 'type' does not exist on type 'number | N... Remove this comment to see the full error message
      (newsItem) => newsItem.type !== NewsTypes.UPDATE
    );
    const orderedNewsWithoutAppUpdates = orderBy(
      filteredNewsWithoutAppUpdates,
      'date',
      'desc'
    );
    const orderedNewsWithAppUpdates = orderBy(
      filteredNewsWithAppUpdates,
      'date',
      'desc'
    );
    runInAction(() => {
      // @ts-ignore ts-migrate(2322) FIXME: Type '(number | News | (() => string) | { (...item... Remove this comment to see the full error message
      this.all = orderedNewsWithoutAppUpdates;
      // @ts-ignore ts-migrate(2322) FIXME: Type '(number | News | (() => string) | { (...item... Remove this comment to see the full error message
      this.allWithAppUpdates = orderedNewsWithAppUpdates;
    });
  }

  @computed
  get incident(): News | null | undefined {
    const incidents = filter(
      this.all,
      (item) => item.type === NewsTypes.INCIDENT
    );
    const lastIncidentIndex =
      incidents.length > 0 ? incidents.length - 1 : null;

    if (lastIncidentIndex !== null) {
      return incidents[lastIncidentIndex];
    }

    return null;
  }

  @computed
  get alerts(): NewsTypesStateType {
    const alerts = filter(this.all, (item) => item.type === NewsTypes.ALERT);
    // Order alerts from newest to oldest
    const orderedAlerts = orderBy(alerts, 'date', 'asc');
    const obj = new NewsCollection(orderedAlerts);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed
  get announcements(): NewsTypesStateType {
    const announcements = filter(
      this.all,
      (item) => item.type === NewsTypes.ANNOUNCEMENT && !item.read
    );
    const obj = new NewsCollection(announcements);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed
  get infos(): NewsTypesStateType {
    const infos = filter(
      this.all,
      (item) => item.type === NewsTypes.INFO && !item.read
    );
    const obj = new NewsCollection(infos);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed
  get unread(): Array<News> {
    const unread = filter(this.all, (item) => !item.read);
    // Order unread from newest to oldest
    return orderBy(unread, 'date', 'asc');
  }

  @computed
  get read(): Array<News> {
    const read = filter(this.all, (item) => item.read);
    // Order read from newest to oldest
    return orderBy(read, 'date', 'asc');
  }

  @computed
  get update(): News | null {
    return this.allWithAppUpdates.filter(
      (item) => item.type === NewsTypes.UPDATE
    )[0];
  }
}

export default {
  News,
  NewsCollection,
};
