// @flow
import { observable, computed } from 'mobx';
import { filter, orderBy } from 'lodash';
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

const { version, platform, platformVersion } = global.environment;

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
  @observable news: Array<News>;

  constructor(data: Array<News>) {
    // Filter news by palatform and versions
    // const filteredNews = filter(
    //   data,
    //   newsItem =>
    //     newsItem.target.daedalus === version &&
    //     newsItem.target.platform === platform &&
    //     newsItem.target.platformVersion === platformVersion
    // );

    Object.assign(this, {
      // Order news from oldest to newest
      all: orderBy(data, 'date', 'desc'),
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
    const orederedAlerts = orderBy(alerts, 'date', 'desc');

    const obj = new NewsCollection(orederedAlerts);
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
    const orederedAnnouncements = orderBy(announcements, 'date', 'desc');
    const obj = new NewsCollection(orederedAnnouncements);
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
    const orederedUnread = orderBy(unread, 'date', 'desc');
    return orederedUnread;
  }

  @computed get read(): Array<News> {
    const read = filter(this.all, item => item.read);
    // Order read from newest to oldest
    const orederedRead = orderBy(read, 'date', 'desc');
    return orederedRead;
  }
}

export default {
  News,
  NewsCollection,
};
