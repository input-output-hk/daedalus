import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import AnalyticsConsentForm from './AnalyticsConsentForm';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';

storiesOf('Analytics', module)
  .addDecorator(withKnobs)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Analytics Consent Form', () => <AnalyticsConsentForm />);
