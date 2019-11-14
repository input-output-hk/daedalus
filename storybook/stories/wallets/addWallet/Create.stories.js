// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import CreateWalletScreens from '../_utils/CreateWalletScreens';

// Assets and helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

storiesOf('Wallets|Add Wallet', module)
  .addDecorator(WalletsWrapper)
  .add('Create', () => <CreateWalletScreens />);
