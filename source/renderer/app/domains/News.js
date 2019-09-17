// @flow
import { observable } from 'mobx';

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

export default class News {
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
