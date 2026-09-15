import React from 'react';
import { action } from '@storybook/addon-actions';
import { withState } from '../../_support/WithLocalState';
import { mockedLocaleState, onLocaleValueChange } from '../utils/helpers';
import StoryDecorator from '../../_support/StoryDecorator';
import InitialSettings from '../../../../source/renderer/app/components/profile/initial-settings/InitialSettings';

export default {
  title: 'Settings / Language',
  decorators: [(story) => <StoryDecorator>{story()}</StoryDecorator>],
};

export const SelectLanguageInitial = withState(mockedLocaleState, (store) => (
  <div>
    <InitialSettings
      onSubmit={action('submit')}
      onChangeItem={(id, value) => onLocaleValueChange(store, id, value)}
      {...store.state}
    />
  </div>
));

SelectLanguageInitial.storyName = 'Select Language - initial';

export const SelectLanguageSubmitting = withState(
  mockedLocaleState,
  (store) => (
    <div>
      <InitialSettings
        onSubmit={action('submit')}
        onChangeItem={(id, value) => onLocaleValueChange(store, id, value)}
        isSubmitting
        {...store.state}
      />
    </div>
  )
);

SelectLanguageSubmitting.storyName = 'Select Language - submitting';
