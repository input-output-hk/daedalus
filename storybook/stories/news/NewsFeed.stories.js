// @flow
// eslint-disable-file no-unused-vars
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean } from '@storybook/addon-knobs';
import StoryDecorator from '../support/StoryDecorator';
import NewsFeed from '../../../source/renderer/app/components/news/NewsFeed';
import News from '../../../source/renderer/app/domains/News';

const news = [
  new News.News({
    title: 'Some title 1 in English',
    content: 'Some title 1 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: 1568650464961,
    type: 'incident',
    read: false,
  }),
  new News.News({
    title: 'Some title 2 in English',
    content: 'Some title 2 in English',
    target: { daedalusVersion: null, platform: 'win32' },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: 1568736864962,
    type: 'incident',
    read: false,
  }),
  new News.News({
    title: 'Some title 3 in English',
    content: 'Some title 3 in English',
    target: { daedalusVersion: null, platform: 'linux' },
    action: { label: 'Check en-US', route: '/wallets' },
    date: 1568823264963,
    type: 'alert',
    read: false,
  }),
  new News.News({
    title: 'Some title 4 in English',
    content: 'Some title 4 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: 1568909664963,
    type: 'alert',
    read: false,
  }),
  new News.News({
    title: 'Some title 5 in English',
    content: 'Some title 5 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: { label: 'Check en-US', route: '/settings' },
    date: 1568996064964,
    type: 'announcement',
    read: false,
  }),
  new News.News({
    title: 'Some title 6 in English',
    content: 'Some title 6 in English',
    target: { daedalusVersion: null, platform: 'win32' },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: 1569082464964,
    type: 'announcement',
    read: false,
  }),
  new News.News({
    title: 'Some title 7 in English',
    content: 'Some title 7 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: { label: 'Check en-US', route: '/settings' },
    date: 1569168864965,
    type: 'info',
    read: false,
  }),
  new News.News({
    title: 'Some title 8 in English',
    content: 'Some title 8 in English',
    target: { daedalusVersion: null, platform: 'linux' },
    action: {
      label: 'Visit en-US',
      url: 'https://iohk.zendesk.com/hc/en-us/articles/',
    },
    date: 1569255264965,
    type: 'info',
    read: false,
  }),
  new News.News({
    title: 'Some title 9 in English',
    content: 'Some title 9 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: {
      label: 'Visit https://markdown-it.github.io/',
      url: 'https://markdown-it.github.io/',
    },
    date: 1569255294965,
    type: 'alert',
    read: false,
  }),
];

const newsCollection = new News.NewsCollection(news);

storiesOf('NewsFeed', module)
  .addDecorator(story => (
    <StoryDecorator>{story(newsCollection)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('NewsFeed - no news items fetched from server', () => (
    <div>
      <NewsFeed
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={undefined}
        newsFeedShowClass={boolean('newsFeedShowClass', true)}
      />
    </div>
  ))

  .add('NewsFeed - news feed empty', () => (
    <div>
      <NewsFeed
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={new News.NewsCollection([])}
        newsFeedShowClass={boolean('newsFeedShowClass2', true)}
      />
    </div>
  ))

  .add('NewsFeed', () => (
    <div>
      <NewsFeed
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={newsCollection}
        newsFeedShowClass={boolean('newsFeedShowClass3', true)}
      />
    </div>
  ));
