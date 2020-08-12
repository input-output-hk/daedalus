// @flow
import { version } from '../../../../package.json';
import News from '../../../../source/renderer/app/domains/News';
import type { NewsType } from '../../../../source/renderer/app/api/news/types';

export const getNewsItem = (
  index: number,
  type: NewsType,
  read?: boolean
): News.News =>
  new News.News({
    id: index,
    title: `Title ${index} for ${
      read ? 'a read' : 'an unread'
    } ${type} in English`,
    content: `Content ${index} in English`,
    target: { daedalusVersion: version, platform: 'darwin' },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: 1568650464961,
    type,
    read: read || false,
  });
