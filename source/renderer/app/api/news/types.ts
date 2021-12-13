import type { Platform } from '../../../../common/types/environment.types';

export type NewsTranslations = {
  'en-US': string;
  'ja-JP': string;
};
export type NewsAction = {
  label: NewsTranslations;
  url?: NewsTranslations;
  route?: string;
  event?: string;
};
export type NewsTarget = {
  daedalusVersion: string | null | undefined;
  platform: string;
};
export type NewsType =
  | 'incident'
  | 'alert'
  | 'announcement'
  | 'info'
  | 'software-update';
export type SoftwareUpdateInfo = {
  version: string;
  hash: string;
  url: string;
};
export type SoftwareUpdate = Record<Platform, SoftwareUpdateInfo>;
export type NewsTimestamp = number;
export type NewsItem = {
  title: NewsTranslations;
  content: NewsTranslations;
  target: NewsTarget;
  action: NewsAction;
  date: NewsTimestamp;
  type: NewsType;
  softwareUpdate?: SoftwareUpdate;
};
export type GetNewsResponse = {
  updatedAt: number;
  items: Array<NewsItem>;
};
export type GetReadNewsResponse = {
  readNewsItems: NewsTimestamp[];
};
export type MarkNewsAsReadResponse = Array<number>;
