// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Screens
import { WalletSettingsScreen } from './WalletSettingsScreen.stories.js';
import './WalletDelete.stories';
import './WalletUnpair.stories';
import './WalletRecoveryPhraseVerification.stories';
import './WalletPublicKey.stories';
import './PublicKeyQRCode.stories';

const decorators = [WalletsWrapper];

/* eslint-disable consistent-return */
storiesOf('Wallets/Settings', module).add(
  'Wallet Settings',
  (_, props) => <WalletSettingsScreen {...props} />,
  {
    decorators,
  }
);
