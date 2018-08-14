// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import BigNumber from 'bignumber.js';
import StoryDecorator from './support/StoryDecorator';
import WalletSummary from '../../source/renderer/app/components/wallet/summary/WalletSummary';

storiesOf('WalletSummary', module)

  .addDecorator((story) => (
    <StoryDecorator>
      {story()}
    </StoryDecorator>
  ))

  // ====== Stories ======

  .add('default', () => (
    <div>
      <WalletSummary
        walletName="Shopping wallet"
        amount="45119903750165.23"
        pendingAmount={{
          total: new BigNumber(3),
          incoming: new BigNumber(1),
          outgoing: new BigNumber(2),
        }}
        numberOfTransactions={20303585}
        isLoadingTransactions={false}
      />
    </div>
  ));
