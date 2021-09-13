// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, boolean, text } from '@storybook/addon-knobs';
import StoryDecorator from '../../_support/StoryDecorator';
import LegacyNotification from '../../../../source/renderer/app/components/notifications/LegacyNotification';

storiesOf('Wallets|Legacy Wallets', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Legacy Notification', () => (
    <div>
      <LegacyNotification
        activeWalletName={text('activeWalletName', 'Josephine')}
        onLearnMore={action('onLearnMore')}
        onTransferFunds={action('onTransferFunds')}
        hasRewardsWallets={boolean('hasRewardsWallets')}
        onWalletAdd={action('onWalletAdd')}
      />
    </div>
  ));
