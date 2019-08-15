// @flow
import React from 'react';
import { boolean } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
// import StoryDecorator from '../_support/StoryDecorator';
import SettingsWrapper from './SettingsWrapper';
import { LANGUAGE_OPTIONS } from '../../../source/renderer/app/config/profileConfig';
import {
  updateParam,
  // onReceiveParam,
} from '../../addons/DaedalusMenu';

// Screens
import GeneralSettings from '../../../source/renderer/app/components/settings/categories/GeneralSettings';

// export const updateParam = (query: Object) =>
//   channel.emit('daedalusMenu/updateParam', query);

const locales = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

/* eslint-disable consistent-return */
storiesOf('SETTINGS|General', module)
  .addDecorator(SettingsWrapper)

  // ====== Stories ======

  .add('General', () => (
    <GeneralSettings
      languages={LANGUAGE_OPTIONS}
      currentLocale="en-US"
      onSelectLanguage={({ locale }) =>
        updateParam({ localeName: locales[locale] })
      }
      isSubmitting={boolean('isSubmitting', false)}
    />
  ));
