import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import AnalyticsDialog from './AnalyticsDialog';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
storiesOf('Analytics', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)
  .add('Analytics Dialog', () => (
    <div
      style={{
        padding: 30,
      }}
    >
      <AnalyticsDialog />
    </div>
  ));
