import React from 'react';
import { storiesOf } from '@storybook/react';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Screens
import { WalletSettingsScreen } from './WalletSettingsScreen.stories';
import './WalletDelete.stories';
import './WalletUnpair.stories';
import './WalletRecoveryPhraseVerification.stories';
import './WalletPublicKey.stories';
import './PublicKeyQRCode.stories';

const decorators = [WalletsWrapper];

/* eslint-disable consistent-return */
storiesOf('Wallets/Settings', module).add(
  'Wallet Settings',
  // @ts-ignore ts-migrate(2741) FIXME: Property 'locale' is missing in type '{ componentI... Remove this comment to see the full error message
  (_, props) => <WalletSettingsScreen {...props} />,
  {
    decorators,
  }
);
