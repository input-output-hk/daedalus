// @flow

export type NewsTranslations = {
  'en-US': string,
  'ja-JP': string,
};

export type NewsAction = {
  label: NewsTranslations,
  url?: NewsTranslations,
  route?: string,
};

export type NewsTarget = {
  daedalus: string,
  platform: string,
  platformVersion: string,
};

export type NewsType = 'incident' | 'alert' | 'announcement' | 'info';

export type NewsItem = {
  title: NewsTranslations,
  content: NewsTranslations,
  target: NewsTarget,
  action:  NewsAction,
  date: Date,
  type: NewsType,
};

export type GetNewsResponse = {
  updatedAt: Date,
  items: Array<NewsItem>,
};
