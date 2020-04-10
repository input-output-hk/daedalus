// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, withKnobs } from '@storybook/addon-knobs';

// Screens
import { action } from '@storybook/addon-actions';
import SetWalletPassword from '../../../../source/renderer/app/components/wallet/settings/SetWalletPassword';
import StoryDecorator from '../../_support/StoryDecorator';

storiesOf('Wallets|Set Password', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('Set Wallet Password', () => (
    <SetWalletPassword
      isSetWalletPasswordDialogOpen={boolean('isSetWalletPasswordDialogOpen', false)}
      onSetWalletPassword={action('onSetWalletPassword')}
    />
  ));
