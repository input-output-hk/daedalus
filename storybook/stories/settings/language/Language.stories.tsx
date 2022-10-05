import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import StoryDecorator from '../../_support/StoryDecorator';
import InitialSettings from '../../../../source/renderer/app/components/profile/initial-settings/InitialSettings';

const onValueChange = (store, id, value) => {
  let key;
  switch (id) {
    case 'dateFormat':
      key = 'currentDateFormat';
      break;
    case 'numberFormat':
      key = 'currentNumberFormat';
      break;
    case 'timeFormat':
      key = 'currentTimeFormat';
      break;
    default:
      key = 'currentLocale';
  }

  store.set({
    [key]: value,
  });
};

storiesOf('Settings / Language', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add(
    'Select Language - initial',
    withState(
      {
        currentDateFormat: 'MM/DD/YYYY',
        currentNumberFormat: 'number-1',
        currentTimeFormat: 'hh:mm:ss A',
        currentLocale: 'en-US',
      },
      (store) => (
        <div>
          <InitialSettings
            onSubmit={action('submit')}
            onChangeItem={(id, value) => onValueChange(store, id, value)}
            currentDateFormat={store.state.currentDateFormat}
            currentLocale={store.state.currentLocale}
            currentNumberFormat={store.state.currentNumberFormat}
            currentTimeFormat={store.state.currentTimeFormat}
          />
        </div>
      )
    )
  )
  .add(
    'Select Language - submitting',
    withState(
      {
        currentDateFormat: 'MM/DD/YYYY',
        currentNumberFormat: 'number-1',
        currentTimeFormat: 'hh:mm:ss A',
        currentLocale: 'en-US',
      },
      (store) => (
        <div>
          <InitialSettings
            onSubmit={action('submit')}
            onChangeItem={(id, value) => onValueChange(store, id, value)}
            currentDateFormat={store.state.currentDateFormat}
            currentLocale={store.state.currentLocale}
            currentNumberFormat={store.state.currentNumberFormat}
            currentTimeFormat={store.state.currentTimeFormat}
            isSubmitting
          />
        </div>
      )
    )
  );
