// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';

// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';

// Screens
import WalletSettingsScreen from './WalletSettingsScreen.stories.js';
import './WalletDelete.stories';
import './WalletRecoveryPhraseVerification.stories';
import './WalletPublicKeyQRCode.stories';

/* eslint-disable consistent-return */
storiesOf('Wallets|Settings', module)
  .addDecorator(WalletsWrapper)
  .add('Wallet Settings', (props) => <WalletSettingsScreen {...props} />);
