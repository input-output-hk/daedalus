// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import CreateWalletScreens from '../utils/CreateWalletScreens';

// Assets and helpers
import WalletsWrapper from '../utils/WalletsWrapper';

storiesOf('Wallets|Actions', module)
  .addDecorator(WalletsWrapper)
  .add('Create', () => <CreateWalletScreens />);
