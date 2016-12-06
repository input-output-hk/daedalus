import React from 'react';
import { ThemeProvider } from 'react-css-themr';
import { storiesOf } from '@kadira/storybook';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../app/themes/daedalus';
import Settings from '../app/components/settings/Settings';
import ProfileSettings from '../app/components/settings/categories/ProfileSettings';
import Profile from '../app/domain/Profile';
import translations from '../app/i18n/translations';

const userProfile = new Profile({
  name: 'Satoshi Nakamoto',
  email: 'satoshi@gmail.com',
  phoneNumber: '‎+810112714444',
  passwordHash: '961b6dd3ede3cb8ecbaacbd68de040cd78eb2ed5889130cceb4c49268ea4d506',
  passwordUpdateDate: '2015-11-20T10:18:06.286Z',
  languageLocale: 'en-US'
});

storiesOf('Settings', module)

  .addDecorator((story) => (
    <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
      <ThemeProvider theme={daedalusTheme}>
        {story()}
      </ThemeProvider>
    </IntlProvider>
  ))

  // ====== Stories ======

  .add('ProfileSettings', () => (
    <Settings>
      <ProfileSettings profile={userProfile} />
    </Settings>
  ));
