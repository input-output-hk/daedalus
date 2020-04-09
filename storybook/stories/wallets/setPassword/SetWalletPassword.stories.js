// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Screens
import SetWalletPassword from '../../../../source/renderer/app/components/wallet/settings/SetWalletPassword';
import StoryDecorator from '../../_support/StoryDecorator';

storiesOf('Wallets|Set Password', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)
  .add('Send', () => (
    <SetWalletPassword onConfirm={() => {}} isDialogOpen={() => {}} />
  ));
