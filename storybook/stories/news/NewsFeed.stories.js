// @flow
// eslint-disable-file no-unused-vars
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean, select, withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import NewsFeed from '../../../source/renderer/app/components/news/NewsFeed';
import News from '../../../source/renderer/app/domains/News';
import { dateOptions } from '../_support/profileSettings';
import { DATE_ENGLISH_OPTIONS } from '../../../source/renderer/app/config/profileConfig';

const news = [
  new News.News({
    id: 1,
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
    id: 2,
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
    id: 3,
    title: 'Some title 3 in English',
    content: 'Some title 3 in English',
    target: { daedalusVersion: null, platform: 'linux' },
    action: { label: 'Check en-US', route: '/wallets' },
    date: 1568823264963,
    type: 'alert',
    read: false,
  }),
  new News.News({
    id: 4,
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
    id: 5,
    title: 'Some title 5 in English',
    content: 'Some title 5 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: { label: 'Check en-US', route: '/settings' },
    date: 1568996064964,
    type: 'announcement',
    read: false,
  }),
  new News.News({
    id: 6,
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
    id: 7,
    title: 'Some title 7 in English',
    content: 'Some title 7 in English',
    target: { daedalusVersion: null, platform: 'darwin' },
    action: { label: 'Check en-US', route: '/settings' },
    date: 1569168864965,
    type: 'info',
    read: false,
  }),
  new News.News({
    id: 8,
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
    id: 9,
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

storiesOf('News|NewsFeed', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))

  // ====== Stories ======

  .add('Empty', () => (
    <div>
      <NewsFeed
        onGoToRoute={action('onGoToRoute')}
        isLoadingNews={false}
        onMarkNewsAsRead={action('onMarkNewsAsRead')}
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={new News.NewsCollection([])}
        isNewsFeedOpen={boolean('isNewsFeedOpen2', true)}
        onOpenExternalLink={action('onOpenExternalLink')}
        onOpenAlert={action('onOpenAlert')}
        onProceedNewsAction={action('onOpenExternalLink')}
        currentDateFormat=" "
      />
    </div>
  ))

  .add('Fetching', () => (
    <div>
      <NewsFeed
        onGoToRoute={action('onGoToRoute')}
        isLoadingNews
        onMarkNewsAsRead={action('onMarkNewsAsRead')}
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={new News.NewsCollection([])}
        isNewsFeedOpen={boolean('isNewsFeedOpen2', true)}
        onOpenExternalLink={action('onOpenExternalLink')}
        onOpenAlert={action('onOpenAlert')}
        onProceedNewsAction={action('onOpenExternalLink')}
        currentDateFormat=" "
      />
    </div>
  ))

  .add('Fetched', () => (
    <div>
      <NewsFeed
        onGoToRoute={action('onGoToRoute')}
        isLoadingNews={false}
        onMarkNewsAsRead={action('onMarkNewsAsRead')}
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={newsCollection}
        isNewsFeedOpen={boolean('isNewsFeedOpen3', true)}
        onOpenExternalLink={action('onOpenExternalLink')}
        onOpenAlert={action('onOpenAlert')}
        onProceedNewsAction={action('onOpenExternalLink')}
        currentDateFormat={select(
          'currentDateFormat',
          dateOptions,
          DATE_ENGLISH_OPTIONS[0].value
        )}
      />
    </div>
  ));
