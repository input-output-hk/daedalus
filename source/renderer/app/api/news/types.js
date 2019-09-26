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
  daedalusVersion: ?string,
  platform: string,
};

export type NewsType = 'incident' | 'alert' | 'announcement' | 'info';

export type NewsTimestamp = number;

export type NewsItem = {
  title: NewsTranslations,
  content: NewsTranslations,
  target: NewsTarget,
  action: NewsAction,
  date: NewsTimestamp,
  type: NewsType,
};

export type GetNewsResponse = {
  updatedAt: number,
  items: Array<NewsItem>,
};

export type GetReadNewsResponse = {
  readNewsItems: NewsTimestamp[],
};

export type MarkNewsAsReadResponse = Array<number>;
