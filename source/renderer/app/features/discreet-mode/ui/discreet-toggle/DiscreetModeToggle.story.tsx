import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import { DiscreetModeToggleComponent } from './DiscreetModeToggle';

storiesOf('Discreet Mode|Discreet Mode Toggle', module)
  .addDecorator(withKnobs)
  .add('Main', () => (
    <div
      style={{
        padding: 20,
      }}
    >
      <div
        style={{
          marginBottom: 20,
        }}
      >
        <DiscreetModeToggleComponent
          onToggle={action('onChange')}
          isDiscreetMode
        />
      </div>
      <div>
        <DiscreetModeToggleComponent
          onToggle={action('onChange')}
          isDiscreetMode={false}
        />
      </div>
    </div>
  ));
