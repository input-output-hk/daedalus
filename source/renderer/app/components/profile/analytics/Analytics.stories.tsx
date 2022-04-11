import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean } from '@storybook/addon-knobs';
import AnalyticsDialog from './AnalyticsDialog';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';
import AnalyticsForm from './AnalyticsForm';

storiesOf('Analytics', module)
  .addDecorator(withKnobs)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Analytics Dialog', () => (
    <div
      style={{
        padding: 30,
      }}
    >
      <AnalyticsDialog />
    </div>
  ));
