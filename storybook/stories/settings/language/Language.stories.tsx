import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withState, Store } from '@dump247/storybook-state';
import {
  DATE_ENGLISH_OPTIONS,
  LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';
import StoryDecorator from '../../_support/StoryDecorator';
import InitialSettings from '../../../../source/renderer/app/components/profile/initial-settings/InitialSettings';

interface StoryStore {
  currentDateFormat: string;
  currentNumberFormat: string;
  currentTimeFormat: string;
  currentLocale: string;
}

const mockedLanguageState = {
  currentDateFormat: DATE_ENGLISH_OPTIONS[0].value,
  currentNumberFormat: NUMBER_OPTIONS[0].value,
  currentTimeFormat: TIME_OPTIONS[0].value,
  currentLocale: LANGUAGE_OPTIONS[0].value,
};

const onValueChange = (
  store: Store<StoryStore>,
  id: string,
  value: string
): void => {
  const fieldIdToStoreKeyMap = {
    dateFormat: 'currentDateFormat',
    numberFormat: 'currentNumberFormat',
    timeFormat: 'currentTimeFormat',
    locale: 'currentLocale',
  };

  store.set({
    [fieldIdToStoreKeyMap[id]]: value,
  });

  if (id === 'locale') {
    const currentDateFormat =
      value === mockedLanguageState.currentLocale
        ? mockedLanguageState.currentDateFormat
        : 'YYYY年MM月DD日';
    store.set({
      currentDateFormat,
    });
  }
};

storiesOf('Settings / Language', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add(
    'Select Language - initial',
    withState(mockedLanguageState, (store) => (
      <div>
        <InitialSettings
          onSubmit={action('submit')}
          onChangeItem={(id, value) => onValueChange(store, id, value)}
          {...store.state}
        />
      </div>
    ))
  )
  .add(
    'Select Language - submitting',
    withState(mockedLanguageState, (store) => (
      <div>
        <InitialSettings
          onSubmit={action('submit')}
          onChangeItem={(id, value) => onValueChange(store, id, value)}
          isSubmitting
          {...store.state}
        />
      </div>
    ))
  );
