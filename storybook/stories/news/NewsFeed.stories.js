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
        news: [
          {
            title: 'Some title 1 in English',
            content: 'Some title 1 in English',
            target: { daedalus: '0.14.0', platform: 'darwin' },
            action: {
              label: 'Visit en-US',
              url: 'https://iohk.zendesk.com/hc/en-us/articles/',
            },
            date: 1568650464961,
            type: 'incident',
            read: false,
          },
          {
            title: 'Some title 2 in English',
            content: 'Some title 2 in English',
            target: { daedalus: '0.13.0', platform: 'win32' },
            action: {
              label: 'Visit en-US',
              url: 'https://iohk.zendesk.com/hc/en-us/articles/',
            },
            date: 1568736864962,
            type: 'incident',
            read: false,
          },
          {
            title: 'Some title 3 in English',
            content: 'Some title 3 in English',
            target: { daedalus: '0.13.0', platform: 'linux' },
            action: { label: 'Check en-US', route: '/wallets' },
            date: 1568823264963,
            type: 'alert',
            read: false,
          },
          {
            title: 'Some title 4 in English',
            content: 'Some title 4 in English',
            target: { daedalus: '0.14.0', platform: 'darwin' },
            action: {
              label: 'Visit en-US',
              url: 'https://iohk.zendesk.com/hc/en-us/articles/',
            },
            date: 1568909664963,
            type: 'alert',
            read: false,
          },
          {
            title: 'Some title 5 in English',
            content: 'Some title 5 in English',
            target: { daedalus: '0.12.0', platform: 'darwin' },
            action: { label: 'Check en-US', route: '/settings' },
            date: 1568996064964,
            type: 'announcement',
            read: false,
          },
          {
            title: 'Some title 6 in English',
            content: 'Some title 6 in English',
            target: { daedalus: '0.13.0', platform: 'win32' },
            action: {
              label: 'Visit en-US',
              url: 'https://iohk.zendesk.com/hc/en-us/articles/',
            },
            date: 1569082464964,
            type: 'announcement',
            read: false,
          },
          {
            title: 'Some title 7 in English',
            content: 'Some title 7 in English',
            target: { daedalus: '0.14.0', platform: 'darwin' },
            action: { label: 'Check en-US', route: '/settings' },
            date: 1569168864965,
            type: 'info',
            read: false,
          },
          {
            title: 'Some title 8 in English',
            content: 'Some title 8 in English',
            target: { daedalus: '0.13.0', platform: 'linux' },
            action: {
              label: 'Visit en-US',
              url: 'https://iohk.zendesk.com/hc/en-us/articles/',
            },
            date: 1569255264965,
            type: 'info',
            read: false,
          },
          {
            title: 'Some title 9 in English',
            content: 'Some title 9 in English',
            target: { daedalus: '0.13.0', platform: 'darwin' },
            action: {
              label: 'Visit https://markdown-it.github.io/',
              url: 'https://markdown-it.github.io/',
            },
            date: 1569255294965,
            type: 'alert',
            read: false,
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
