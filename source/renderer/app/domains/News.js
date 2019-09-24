// @flow
import { observable, computed, runInAction } from 'mobx';
import { get, filter, orderBy, includes } from 'lodash';
import semver from 'semver';
import type { NewsTarget, NewsType } from '../api/news/types';

export type NewsAction = {
  label: string,
  url?: string,
  route?: string,
};

export const NewsTypes: {
  INCIDENT: NewsType,
  ALERT: NewsType,
  ANNOUNCEMENT: NewsType,
  INFO: NewsType,
} = {
  INCIDENT: 'incident',
  ALERT: 'alert',
  ANNOUNCEMENT: 'announcement',
  INFO: 'info',
};

export type NewsTypesStateType = {
  all: Array<News>,
  unread: Array<News>,
  read: Array<News>,
};

const { version, platform } = global.environment;

class News {
  @observable title: string;
  @observable content: string;
  @observable target: NewsTarget;
  @observable action: NewsAction;
  @observable date: number;
  @observable type: NewsType;
  @observable read: boolean;

  constructor(data: {
    title: string,
    content: string,
    target: NewsTarget,
    action: NewsAction,
    date: number,
    type: NewsType,
    read: boolean,
  }) {
    Object.assign(this, data);
  }
}

class NewsCollection {
  @observable all: Array<News> = [];

  constructor(data: Array<News>) {
    // Filter news by palatform and versions
    const filteredNews = filter(data, newsItem => {
      const availableTargetVersionRange = get(
        newsItem,
        ['target', 'daedalusVersion'],
        null
      );
      const targetPlatforms = get(newsItem, ['target', 'platforms']);
      return (
        (!availableTargetVersionRange ||
          (availableTargetVersionRange &&
            semver.satisfies(version, availableTargetVersionRange))) &&
        (platform === 'browser' || includes(targetPlatforms, platform))
      );
    });
    const orderedNews = orderBy(filteredNews, 'date', 'desc');
    runInAction(() => {
      this.all = orderedNews;
    });
  }

  @computed get incident(): ?News {
    const incidents = filter(
      this.all,
      item => item.type === NewsTypes.INCIDENT
    );
    const lastIncidentIndex =
      incidents.length > 0 ? incidents.length - 1 : null;

    if (lastIncidentIndex !== null) {
      return incidents[lastIncidentIndex];
    }
    return null;
  }

  @computed get alerts(): NewsTypesStateType {
    const alerts = filter(this.all, item => item.type === NewsTypes.ALERT);
    // Order alerts from newest to oldest
    const orderedAlerts = orderBy(alerts, 'date', 'asc');

    const obj = new NewsCollection(orderedAlerts);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed get announcements(): NewsTypesStateType {
    const announcements = filter(
      this.all,
      item => item.type === NewsTypes.ANNOUNCEMENT && !item.read
    );
    const obj = new NewsCollection(announcements);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed get infos(): NewsTypesStateType {
    const infos = filter(
      this.all,
      item => item.type === NewsTypes.INFO && !item.read
    );

    const obj = new NewsCollection(infos);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed get unread(): Array<News> {
    const unread = filter(this.all, item => !item.read);
    // Order unread from newest to oldest
    return orderBy(unread, 'date', 'asc');
  }

  @computed get read(): Array<News> {
    const read = filter(this.all, item => item.read);
    // Order read from newest to oldest
    return orderBy(read, 'date', 'asc');
  }
}

export default {
  News,
  NewsCollection,
};
