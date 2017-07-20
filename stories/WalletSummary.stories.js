import React from 'react';
import { storiesOf } from '@kadira/storybook';
import StoryDecorator from './support/StoryDecorator';
import WalletSummary from '../app/components/wallet/summary/WalletSummary';
import BigNumber from 'bignumber.js';

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
        amount={45119903750165.23}
        pendingAmount={{
          incoming: new BigNumber(1),
          outgoing: new BigNumber(2),
        }}
        numberOfTransactions={20303585}
      />
    </div>
  ));
