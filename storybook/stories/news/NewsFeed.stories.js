// @flow
// eslint-disable-file no-unused-vars
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import StoryDecorator from '../support/StoryDecorator';
import NewsFeed from '../../../source/renderer/app/components/news/NewsFeed';

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>
      {story({
        updatedAt: 1568650464961,
        items: [
          {
            title: {
              'en-US': 'Some title 1 in English',
              'ja-JP': 'Some title 1 in Japanese',
            },
            content: {
              'en-US': 'Content 1 in English',
              'ja-JP': 'Content 1 in Japanese',
            },
            target: {
              daedalus: '0.14.0',
              platform: 'darwin',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Visit en-US',
                'ja-JP': 'Visit ja-JP',
              },
              url: {
                'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
                'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
              },
            },
            date: 1568650464961,
            type: 'incident',
          },
          {
            title: {
              'en-US': 'Some title 2 in English',
              'ja-JP': 'Some title 2 in Japanese',
            },
            content: {
              'en-US': 'Content 2 in English',
              'ja-JP': 'Content 2 in Japanese',
            },
            target: {
              daedalus: '0.13.0',
              platform: 'win32',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Visit en-US',
                'ja-JP': 'Visit ja-JP',
              },
              url: {
                'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
                'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
              },
            },
            date: 1568736864962,
            type: 'incident',
          },
          {
            title: {
              'en-US': 'Some title 3 in English',
              'ja-JP': 'Some title 3 in Japanese',
            },
            content: {
              'en-US': 'Content 3 in English',
              'ja-JP': 'Content 3 in Japanese',
            },
            target: {
              daedalus: '0.13.0',
              platform: 'linux',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Check en-US',
                'ja-JP': 'Check ja-JP',
              },
              route: '/wallets',
            },
            date: 1568823264963,
            type: 'alert',
          },
          {
            title: {
              'en-US': 'Some title 4 in English',
              'ja-JP': 'Some title 4 in Japanese',
            },
            content: {
              'en-US': 'Content 4 in English',
              'ja-JP': 'Content 4 in Japanese',
            },
            target: {
              daedalus: '0.14.0',
              platform: 'darwin',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Visit en-US',
                'ja-JP': 'Visit ja-JP',
              },
              url: {
                'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
                'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
              },
            },
            date: 1568909664963,
            type: 'alert',
          },
          {
            title: {
              'en-US': 'Some title 5 in English',
              'ja-JP': 'Some title 5 in Japanese',
            },
            content: {
              'en-US': 'Content 5 in English',
              'ja-JP': 'Content 5 in Japanese',
            },
            target: {
              daedalus: '0.12.0',
              platform: 'darwin',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Check en-US',
                'ja-JP': 'Check ja-JP',
              },
              route: '/settings',
            },
            date: 1568996064964,
            type: 'announcement',
          },
          {
            title: {
              'en-US': 'Some title 6 in English',
              'ja-JP': 'Some title 6 in Japanese',
            },
            content: {
              'en-US': 'Content 6 in English',
              'ja-JP': 'Content 6 in Japanese',
            },
            target: {
              daedalus: '0.13.0',
              platform: 'win32',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Visit en-US',
                'ja-JP': 'Visit ja-JP',
              },
              url: {
                'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
                'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
              },
            },
            date: 1569082464964,
            type: 'announcement',
          },
          {
            title: {
              'en-US': 'Some title 7 in English',
              'ja-JP': 'Some title 7 in Japanese',
            },
            content: {
              'en-US': 'Content 7 in English',
              'ja-JP': 'Content 7 in Japanese',
            },
            target: {
              daedalus: '0.14.0',
              platform: 'darwin',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Check en-US',
                'ja-JP': 'Check ja-JP',
              },
              route: '/settings',
            },
            date: 1569168864965,
            type: 'info',
          },
          {
            title: {
              'en-US': 'Some title 8 in English',
              'ja-JP': 'Some title 8 in Japanese',
            },
            content: {
              'en-US': 'Content 8 in English',
              'ja-JP': 'Content 8 in Japanese',
            },
            target: {
              daedalus: '0.13.0',
              platform: 'linux',
              platformVersion: '17.7.0',
            },
            action: {
              label: {
                'en-US': 'Visit en-US',
                'ja-JP': 'Visit ja-JP',
              },
              url: {
                'en-US': 'https://iohk.zendesk.com/hc/en-us/articles/',
                'ja-JP': 'https://iohk.zendesk.com/hc/ja/articles/',
              },
            },
            date: 1569255264965,
            type: 'info',
          },
        ],
      })}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('NewsFeed - no news items fetched from server', () => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
        news={undefined}
        newsFeedShowClass={boolean('newsFeedShowClass', true)}
      />
    </div>
  ))

  .add('NewsFeed - news feed empty', () => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
        news={[]}
        newsFeedShowClass={boolean('newsFeedShowClass2', true)}
      />
    </div>
  ))

  .add('NewsFeed', props => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
        news={props.items}
        newsFeedShowClass={boolean('newsFeedShowClass3', true)}
      />
    </div>
  ));
