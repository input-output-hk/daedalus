// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import StoryDecorator from '../../_support/StoryDecorator';
import ConnectHardwareWallet from '../../../../source/renderer/app/components/hardware-wallet/settings/ConnectHardwareWallet';

storiesOf('Wallets|Hardware Wallets', module)
  .addDecorator(story => <StoryDecorator>{story()}</StoryDecorator>)

  // ====== Stories ======

  .add('Hardware wallet connect step 1', () => (
    <div>
      <ConnectHardwareWallet
        onOpenExternalLink={action('onOpenExternalLink')}
      />
    </div>
  ))

  .add('Hardware wallet connect step 2', () => (
    <div>
      <ConnectHardwareWallet
        onOpenExternalLink={action('onOpenExternalLink')}
      />
    </div>
  ));
