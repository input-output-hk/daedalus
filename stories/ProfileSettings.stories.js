import React from 'react';
import { ThemeProvider } from 'react-css-themr';
import { storiesOf } from '@kadira/storybook';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../app/themes/daedalus';
import Settings from '../app/components/settings/Settings';
import ProfileSettings from '../app/components/settings/categories/ProfileSettings';
import profile from '../app/api/data/account.json';
import UserProfile from '../app/domain/UserProfile';
import translations from '../app/i18n/translations';

const userProfile = new UserProfile(profile);

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
