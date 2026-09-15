import React from 'react';
// Helpers
import WalletsWrapper from '../_utils/WalletsWrapper';
// Screens
import WalletSettingsScreen from './_support/WalletSettingsScreen';
import { localeOf } from '../../_support/globals';

export default {
  title: 'Wallets / Settings',
  decorators: [WalletsWrapper],
};

export const WalletSettings = {
  render: (_args, context) => (
    <WalletSettingsScreen locale={localeOf(context)} />
  ),
};
