// @flow
import { observable, computed, runInAction } from 'mobx';
import { get, filter, orderBy } from 'lodash';
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

export type MethodStateReturnType = {
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
  @observable date: Date;
  @observable type: NewsType;
  @observable read: boolean;

  constructor(data: {
    title: string,
    content: string,
    target: NewsTarget,
    action: NewsAction,
    date: Date,
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
      const targetPlatform = get(newsItem, ['target', 'platform']);
      return (
        (!availableTargetVersionRange ||
          (availableTargetVersionRange &&
            semver.satisfies(version, availableTargetVersionRange))) &&
        targetPlatform === platform
      );
    });

    runInAction(() => {
      this.all.replace(orderBy(data, 'date', 'asc'));
    });
  }

  @computed get incident(): ?MethodStateReturnType {
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

  @computed get alerts(): MethodStateReturnType {
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

  @computed get announcements(): MethodStateReturnType {
    const announcements = filter(
      this.all,
      item => item.type === NewsTypes.ANNOUNCEMENT && !item.read
    );
    // Order announcements from newest to oldest
    const orderedAnnouncements = orderBy(announcements, 'date', 'desc');
    const obj = new NewsCollection(orderedAnnouncements);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed get infos(): MethodStateReturnType {
    const infos = filter(
      this.all,
      item => item.type === NewsTypes.INFO && !item.read
    );
    // Order infos from newest to oldest
    const orederedInfos = orderBy(infos, 'date', 'desc');

    const obj = new NewsCollection(orederedInfos);
    return {
      all: obj.all,
      unread: obj.unread,
      read: obj.read,
    };
  }

  @computed get unread(): Array<News> {
    const unread = filter(this.all, item => !item.read);
    // Order unread from newest to oldest
    return orderBy(unread, 'date', 'desc');
  }

  @computed get read(): Array<News> {
    const read = filter(this.all, item => item.read);
    // Order read from newest to oldest
    return orderBy(read, 'date', 'desc');
  }
}

export default {
  News,
  NewsCollection,
};
