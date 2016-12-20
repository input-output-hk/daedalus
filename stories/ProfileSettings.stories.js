import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import Settings from '../app/components/settings/SettingsLayout';
import ProfileSettings from '../app/components/settings/categories/ProfileSettings';
import Profile from '../app/domain/Profile';

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
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('ProfileSettings', () => (
    <Settings>
      <ProfileSettings profile={userProfile} />
    </Settings>
  ));
