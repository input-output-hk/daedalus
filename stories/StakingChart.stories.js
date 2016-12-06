import React from 'react';
import { ThemeProvider } from 'react-css-themr';
import { storiesOf, action } from '@kadira/storybook';
import { IntlProvider } from 'react-intl';
import { observable } from 'mobx';
import { daedalusTheme } from '../app/themes/daedalus';
import translations from '../app/i18n/translations';
import StakingChart from '../app/components/staking/StakingChart';

const generateRandomSlots = (count:number) => {
  const slots = [];
  for (let i = 0; i < count; i += 1) {
    const transactions = i < (count / 2) ? Math.floor(Math.random() * 50) : 0;
    slots.push({ transactions, slot: slots.length + 1 });
  }
  return slots;
};

storiesOf('StakingChart', module)

  .addDecorator((story) => (
    <IntlProvider {...{ locale: 'en-US', key: 'en-US', messages: translations['en-US'] }}>
      <ThemeProvider theme={daedalusTheme}>
        {story()}
      </ThemeProvider>
    </IntlProvider>
  ))

  // ====== Stories ======

  .add('Default', () => {
    const options = observable({
      data: generateRandomSlots(30),
      ticks: [0, 10, 20, 30, 40, 50],
      activeIndex: null
    });
    return (
      <StakingChart
        options={options}
        onBarClick={(data, index) => {
          action('onBarClick')(data, index);
          options.activeIndex = index;
        }}
      />
    );
  });
