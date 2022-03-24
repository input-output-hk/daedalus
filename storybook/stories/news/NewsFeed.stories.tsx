// eslint-disable-file no-unused-vars
import React from 'react';
// import { omit } from 'lodash';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { boolean, select, number, withKnobs } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import NewsFeed from '../../../source/renderer/app/components/news/NewsFeed';
import News from '../../../source/renderer/app/domains/News';
import { dateOptions } from '../_support/profileSettings';
import { DATE_ENGLISH_OPTIONS } from '../../../source/renderer/app/config/profileConfig';
import { getNewsItem } from './_utils/fakeDataNewsFeed';

const updateDownloadProgressOptions = {
  range: true,
  min: 0,
  max: 100,
  step: 1,
};
storiesOf('News|NewsFeed', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  )) // ====== Stories ======
  .add('Empty', () => (
    <div>
      <NewsFeed
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onGoToRoute={action('onGoToRoute')}
        isLoadingNews={false}
        onMarkNewsAsRead={action('onMarkNewsAsRead')}
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={new News.NewsCollection([])}
        isNewsFeedOpen={boolean('isNewsFeedOpen', true)}
        onOpenExternalLink={action('onOpenExternalLink')}
        onOpenAlert={action('onOpenAlert')}
        onProceedNewsAction={action('onOpenExternalLink')}
        onOpenAppUpdate={action('onOpenAppUpdate')}
        currentDateFormat=" "
        isUpdatePostponed={false}
      />
    </div>
  ))
  .add('Fetching', () => (
    <div>
      <NewsFeed
        // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
        onGoToRoute={action('onGoToRoute')}
        isLoadingNews
        onMarkNewsAsRead={action('onMarkNewsAsRead')}
        onNewsItemActionClick={action('onNewsItemActionClick')}
        onClose={action('onClose')}
        news={new News.NewsCollection([])}
        isNewsFeedOpen={boolean('isNewsFeedOpen', true)}
        onOpenExternalLink={action('onOpenExternalLink')}
        onOpenAlert={action('onOpenAlert')}
        onProceedNewsAction={action('onOpenExternalLink')}
        onOpenAppUpdate={action('onOpenAppUpdate')}
        currentDateFormat=" "
        isUpdatePostponed={false}
      />
    </div>
  ))
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ locale }: { locale: string; }... Remove this comment to see the full error message
  .add('Fetched', ({ locale }: { locale: string }) => {
    const displayAppUpdateNewsItem = boolean('displayAppUpdateNewsItem', true);
    const updateDownloadProgress = displayAppUpdateNewsItem
      ? number('updateDownloadProgress', 30, updateDownloadProgressOptions)
      : 0;
    const news = new News.NewsCollection([
      getNewsItem(1, 'incident', locale),
      getNewsItem(2, 'incident', locale, true),
      getNewsItem(3, 'alert', locale),
      getNewsItem(4, 'alert', locale, true),
      getNewsItem(5, 'announcement', locale),
      getNewsItem(6, 'announcement', locale, true),
      getNewsItem(7, 'info', locale),
      getNewsItem(8, 'info', locale, true),
      getNewsItem(9, 'software-update', locale, true),
    ]);
    return (
      <div>
        <NewsFeed
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          onGoToRoute={action('onGoToRoute')}
          isLoadingNews={false}
          onMarkNewsAsRead={action('onMarkNewsAsRead')}
          onNewsItemActionClick={action('onNewsItemActionClick')}
          onClose={action('onClose')}
          news={news}
          isNewsFeedOpen={boolean('isNewsFeedOpen', true)}
          onOpenExternalLink={action('onOpenExternalLink')}
          onOpenAlert={action('onOpenAlert')}
          onProceedNewsAction={action('onOpenExternalLink')}
          displayAppUpdateNewsItem={displayAppUpdateNewsItem}
          updateDownloadProgress={updateDownloadProgress}
          onOpenAppUpdate={action('onOpenAppUpdate')}
          currentDateFormat={select(
            'currentDateFormat',
            dateOptions,
            DATE_ENGLISH_OPTIONS[0].value
          )}
          isUpdatePostponed={false}
        />
      </div>
    );
  });
