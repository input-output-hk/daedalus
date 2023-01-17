import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import { mockedLocaleState, onLocaleValueChange } from '../utils/helpers';
import StoryDecorator from '../../_support/StoryDecorator';
import InitialSettings from '../../../../source/renderer/app/components/profile/initial-settings/InitialSettings';

storiesOf('Settings / Language', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add(
    'Select Language - initial',
    withState(mockedLocaleState, (store) => (
      <div>
        <InitialSettings
          onSubmit={action('submit')}
          onChangeItem={(id, value) => onLocaleValueChange(store, id, value)}
          {...store.state}
        />
      </div>
    ))
  )
  .add(
    'Select Language - submitting',
    withState(mockedLocaleState, (store) => (
      <div>
        <InitialSettings
          onSubmit={action('submit')}
          onChangeItem={(id, value) => onLocaleValueChange(store, id, value)}
          isSubmitting
          {...store.state}
        />
      </div>
    ))
  );
