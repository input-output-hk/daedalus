// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import StoryDecorator from '../../_support/StoryDecorator';

// Screens
import SetWalletPassword from "../../../../source/renderer/app/components/wallet/settings/SetWalletPassword";

storiesOf('Set Wallet', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Set Wallet Password', () => (
    <div>
      <SetWalletPassword
        onConfirm={() => {}}
        isDialogOpen={() => {}}
      />
    </div>
  ));
