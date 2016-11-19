import React from 'react';
import { ThemeProvider } from 'react-css-themr';
import { storiesOf } from '@kadira/storybook';
import { daedalusTheme } from '../app/themes/daedalus';
import Settings from '../app/components/settings/Settings';
import ProfileSettings from '../app/components/settings/categories/ProfileSettings';

storiesOf('Settings', module)

  .addDecorator((story) => (<div>{story()}</div>))

  // ====== Stories ======

  .add('ProfileSettings', () => (
    <ThemeProvider theme={daedalusTheme}>
      <Settings>
        <ProfileSettings />
      </Settings>
    </ThemeProvider>
  ));
