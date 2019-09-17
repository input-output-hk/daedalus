// @flow
// eslint-disable-file no-unused-vars
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from "@storybook/addon-actions";
import { boolean } from "@storybook/addon-knobs";
import StoryDecorator from '../support/StoryDecorator';
import NewsFeed from '../../../source/renderer/app/components/news/NewsFeed';

storiesOf('NewsFeed', module)
  .addDecorator(story => <StoryDecorator>{story({
    news: {
      updatedAt: 1568650464961,
      items: [{
        title: {
          "en-US": "Some title 1 in English",
          "ja-JP": "Some title 1 in Japanese"
        },
        content: {
          "en-US": "Content 1 in English",
          "ja-JP": "Content 1 in Japanese"
        },
        target: {
          daedalus: "0.14.0",
          platform: "darwin",
          platformVersion: "17.7.0"
        },
        action: {
          label: {
            "en-US": "Visit en-US",
            "ja-JP": "Visit ja-JP"
          },
          url: {
            "en-US": "https://iohk.zendesk.com/hc/en-us/articles/",
            "ja-JP": "https://iohk.zendesk.com/hc/ja/articles/"
          }
        },
        date: 1568650464961,
        type: "incident"
      }],
    },
    }
  )}</StoryDecorator>)

  // ====== Stories ======

  .add('NewsFeed - no news items fetched from server', () => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
        news={{items:[]}}
        noFetchedData={boolean('noFetchedData', true)}
      />
    </div>
  ))

  .add('NewsFeed - news feed empty', () => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
        news={{items:[]}}
        noFetchedData={boolean('noFetchedDataSecond', false)}
      />
    </div>
  ))

  .add('NewsFeed', (props) => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
        news={props.news}
        noFetchedData={boolean('noFetchedDataThird', false)}
      />
    </div>
  ));

