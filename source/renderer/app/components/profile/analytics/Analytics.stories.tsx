import React from 'react';
import { withKnobs } from '@storybook/addon-knobs';
import AnalyticsConsentForm from './AnalyticsConsentForm';
import StoryDecorator from '../../../../../../storybook/stories/_support/StoryDecorator';

export default {
  title: 'Analytics',
  decorators: [
    withKnobs,
    (story) => <StoryDecorator>{story()}</StoryDecorator>,
  ],
};

export const _AnalyticsConsentForm = () => <AnalyticsConsentForm />;
