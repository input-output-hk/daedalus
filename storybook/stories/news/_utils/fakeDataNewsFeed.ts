import { version } from '../../../../package.json';
import News from '../../../../source/renderer/app/domains/News';
import { update } from './fakeDataUpdate';
import type { NewsType } from '../../../../source/renderer/app/api/news/types';

export const getNewsItem = (
  id: number,
  type: NewsType,
  locale: string,
  read?: boolean
  // @ts-ignore ts-migrate(2503) FIXME: Cannot find namespace 'News'.
): News.News =>
  new News.News({
    id,
    title:
      type === 'software-update'
        ? update[locale].title
        : `Title - ${type} - ${locale} - ${read ? 'read' : 'unread'}`,
    content:
      type === 'software-update'
        ? update[locale].content
        : `Content - ${locale}`,
    target: {
      daedalusVersion: version,
      platform: 'darwin',
    },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: new Date().getTime() - 100 - id,
    type,
    read: read || false,
  });
