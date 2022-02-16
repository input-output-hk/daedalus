import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import SecuritySettingsPage from '../../../containers/settings/categories/SecuritySettingsPage';
import { browserLocalStorage } from '../../../environment/localstorage/browser';
import { DiscreetMode } from '../feature';
import { DiscreetValue } from './discreet-value/DiscreetValue';
import { DiscreetModeToggle } from './discreet-toggle/DiscreetModeToggle';
import { EnvironmentProvider } from '../../../environment';

storiesOf('Discreet Mode', module)
  .addDecorator(withKnobs)
  .add('Main', () => (
    <EnvironmentProvider
      environment={{
        storage: browserLocalStorage,
        config: global.environment,
      }}
    >
      <DiscreetMode.Provider>
        <SecuritySettingsPage />
        <DiscreetValue>123</DiscreetValue>
        <DiscreetModeToggle className="discreet-mode-toggle" />
      </DiscreetMode.Provider>
    </EnvironmentProvider>
  ));
