// @flow

export type NewsTranslations = {
  'en-US': string,
  'ja-JP': string,
};

export type NewsItem = {
  title: NewsTranslations,
  content: NewsTranslations,
  target: {
    daedalus: string,
    platform: string,
    platformVersion: string,
  },
  action: {
    label: NewsTranslations,
    url?: NewsTranslations,
    route?: string,
  },
  date: Date,
  type: string,
};

export type GetNewsResponse = {
  updatedAt: Date,
  items: Array<NewsItem>,
};
