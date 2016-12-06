import React from 'react';
import { ThemeProvider } from 'react-css-themr';
import { storiesOf } from '@kadira/storybook';
import { IntlProvider } from 'react-intl';
import { daedalusTheme } from '../app/themes/daedalus';
import Staking from '../app/components/staking/Staking';
import StakingSwitch from '../app/components/staking/StakingSwitch';
import translations from '../app/i18n/translations';

storiesOf('Staking', module)

  .addDecorator((story) => (
    <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
      <ThemeProvider theme={daedalusTheme}>
        {story()}
      </ThemeProvider>
    </IntlProvider>
  ))

  // ====== Stories ======

  .add('Switches', () => (
    <div>
      <StakingSwitch active={false} />
      <StakingSwitch active />
    </div>
  ))

.add('StakingPage', () => (
  <div>
    <Staking />
  </div>
));
