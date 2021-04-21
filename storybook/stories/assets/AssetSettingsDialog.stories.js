// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, number } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import AssetSettingsDialog from '../../../source/renderer/app/components/assets/AssetSettingsDialog';

storiesOf('Assets|AssetSettingsDialog', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Default', () => (
    <AssetSettingsDialog
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
    />
  ))

  .add('With recommended decimal precision', () => (
    <AssetSettingsDialog
      onSubmit={action('onSubmit')}
      onCancel={action('onCancel')}
      recommendedDecimalPrecision={number('recommendedDecimalPrecision')}
    />
  ));
