// @flow
import React from 'react';
import { boolean } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import SettingsWrapper from './SettingsWrapper';
import { LANGUAGE_OPTIONS } from '../../../source/renderer/app/config/profileConfig';
import { updateParam } from '../../addons/DaedalusMenu';
import { locales } from '../_support/config';

// Screens
import GeneralSettings from '../../../source/renderer/app/components/settings/categories/GeneralSettings';

const localeNames = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

/* eslint-disable consistent-return */
storiesOf('SETTINGS|Settings', module)
  .addDecorator(SettingsWrapper)

  // ====== Stories ======

  .add('General', () => (
    <GeneralSettings
      languages={LANGUAGE_OPTIONS}
      currentLocale={locales[sessionStorage.localeName]}
      onSelectLanguage={({ locale }) =>
        updateParam({ param: 'localeName', value: localeNames[locale] })
      }
      isSubmitting={boolean('isSubmitting', false)}
    />
  ));
