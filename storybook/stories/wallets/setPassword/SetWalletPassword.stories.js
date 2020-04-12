// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, withKnobs } from '@storybook/addon-knobs';

// Screens
import SetWalletPassword from '../../../../source/renderer/app/components/wallet/settings/SetWalletPassword';
import StoryDecorator from '../../_support/StoryDecorator';
import ChangeSpendingPasswordDialog
  from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import {generateWallet} from '../../_support/utils';
import STAKE_POOLS from '../../../../source/renderer/app/config/stakingStakePools.dummy';
import Wallet from '../../../../source/renderer/app/domains/Wallet';

const WALLETS = [
  generateWallet('First Wallet', '1000000000', 0, STAKE_POOLS[0], false),
];

const activeWallet: Wallet = WALLETS[0];

let walletPasswordIsOpen: boolean = false;

storiesOf('Wallets|Set Password', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('Set Wallet Password', () => (
    <>
    <SetWalletPassword
      isSetWalletPasswordDialogOpen={false}
      onSetWalletPassword={() => {walletPasswordIsOpen = !walletPasswordIsOpen}}
    />
    {boolean('isSetWalletPasswordDialogOpen', false) && (
      <ChangeSpendingPasswordDialog
        isSpendingPasswordSet={activeWallet.hasPassword}
        currentPasswordValue={''}
        newPasswordValue={''}
        repeatedPasswordValue={''}
        onSave={() => {}}
        onCancel={() => {}}
        onDataChange={() => {}}
        isSubmitting={false}
        error={false}
        walletName={activeWallet.name}
      />
    )}
    </>
  ));
