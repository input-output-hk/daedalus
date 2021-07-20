// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { action } from '@storybook/addon-actions';
import { withKnobs, select } from '@storybook/addon-knobs';
import StoryDecorator from '../_support/StoryDecorator';
import DappTransactionRequest from '../../../source/renderer/app/components/dapp/DappTransactionRequest';
import { WALLETS_V2 } from '../_support/StoryProvider';

storiesOf('dApps|TransactionRequest', module)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>)
  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Request', () => (
    <DappTransactionRequest
      onClose={action('onClose')}
      onSubmit={action('onSubmit')}
      triggedFrom={select(
        'triggedFrom',
        {
          safari: 'safari',
          shrome: 'chrome',
        },
        'safari'
      )}
      wallets={WALLETS_V2}
    />
  ));
