// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import InitialSettings from '../../../../source/renderer/app/components/profile/initial-settings/InitialSettings';
import {
  DATE_ENGLISH_OPTIONS,
  NUMBER_OPTIONS,
  TIME_OPTIONS,
} from '../../../../source/renderer/app/config/profileConfig';

storiesOf('Settings|Language', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Select Language - initial', ({ locale }: { locale: string }) => (
    <div>
      <InitialSettings
        onSubmit={action('submit')}
        onChangeItem={action('onChangeItem')}
        currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentLocale={locale}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
      />
    </div>
  ))

  .add('Select Language - submitting', ({ locale }: { locale: string }) => (
    <div>
      <InitialSettings
        onSubmit={action('submit')}
        onChangeItem={action('onChangeItem')}
        currentDateFormat={DATE_ENGLISH_OPTIONS[0].value}
        currentLocale={locale}
        currentNumberFormat={NUMBER_OPTIONS[0].value}
        currentTimeFormat={TIME_OPTIONS[0].value}
        isSubmitting
      />
    </div>
  ));
