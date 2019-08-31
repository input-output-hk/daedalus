// @flow
import React from 'react';
import { findKey } from 'lodash';
import { boolean, text } from '@storybook/addon-knobs';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import SettingsWrapper from './SettingsWrapper';
import { LANGUAGE_OPTIONS } from '../../../source/renderer/app/config/profileConfig';
import { updateParam /* onReceiveParam */ } from '../../addons/DaedalusMenu';
import { locales, themesIds } from '../_support/config';

// Screens
import GeneralSettings from '../../../source/renderer/app/components/settings/categories/GeneralSettings';
import DisplaySettings from '../../../source/renderer/app/components/settings/categories/DisplaySettings';
import SupportSettings from '../../../source/renderer/app/components/settings/categories/SupportSettings';
import TermsOfUseSettings from '../../../source/renderer/app/components/settings/categories/TermsOfUseSettings';

const getParamName = (obj, itemName): any =>
  Object.entries(obj).find((entry: [any, any]) => itemName === entry[1]);

/* eslint-disable consistent-return */
storiesOf('SETTINGS|Settings', module)
  .addDecorator(SettingsWrapper)

  // ====== Stories ======

  .add('General', () => (
    <GeneralSettings
      languages={LANGUAGE_OPTIONS}
      currentLocale="English"
      onSelectLanguage={({ locale }) =>
        updateParam({
          param: 'localeName',
          value: findKey(locales, item => item === locale),
        })
      }
      isSubmitting={boolean('isSubmitting', false)}
    />
  ))
  .add('Themes', () => (
    <DisplaySettings
      theme="DarkBlue"
      selectTheme={({ theme }) => {
        updateParam({
          param: 'themeName',
          value: getParamName(themesIds, theme)[0],
        });
      }}
    />
  ))
  .add('Terms of use', () => (
    <TermsOfUseSettings
      localizedTermsOfUse={text('Terms os use:', 'Terms os use...')}
    />
  ))
  .add('Support', () => (
    <SupportSettings
      onExternalLinkClick={action('onExternalLinkClick')}
      onSupportRequestClick={action('onSupportRequestClick')}
      onDownloadLogs={action('onDownloadLogs')}
      disableDownloadLogs={boolean('disableDownloadLogs', false)}
    />
  ));
