// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import InitialSettings from '../../../../source/renderer/app/components/profile/initial-settings/InitialSettings';
import {
  DATE_ENGLISH_OPTIONS,
  DATE_JAPANESE_OPTIONS,
  LANGUAGE_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';

storiesOf('Settings|Language', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Select Language - initial', () => (
    <div>
      <InitialSettings
        onSubmit={action('submit')}
        onChangeItem={action('onChangeItem')}
        currentDateEnglishFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentDateJapaneseFormat={DATE_JAPANESE_OPTIONS[0].value}
        currentLocale={LANGUAGE_OPTIONS[0].value}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
      />
    </div>
  ))

  .add('Select Language - submitting', () => (
    <div>
      <InitialSettings
        onSubmit={action('submit')}
        onChangeItem={action('onChangeItem')}
        currentDateEnglishFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentDateJapaneseFormat={DATE_JAPANESE_OPTIONS[0].value}
        currentLocale={LANGUAGE_OPTIONS[0].value}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
        isSubmitting
      />
    </div>
  ));
