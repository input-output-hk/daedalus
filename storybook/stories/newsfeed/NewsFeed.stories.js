// @flow
// eslint-disable-file no-unused-vars
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from "@storybook/addon-actions";
import StoryDecorator from '../support/StoryDecorator';
import NewsFeed from '../../../source/renderer/app/components/news/NewsFeed';

storiesOf('NewsFeed', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('NewsFeed', () => (
    <div>
      <NewsFeed
        onClose={action('onClose')}
      />
    </div>
  ));
