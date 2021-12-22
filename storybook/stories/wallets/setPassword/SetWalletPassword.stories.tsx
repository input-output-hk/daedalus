import React from 'react';
import { storiesOf } from '@storybook/react';
import { boolean, withKnobs } from '@storybook/addon-knobs';
import BigNumber from 'bignumber.js';
// Screens
import SetWalletPassword from '../../../../source/renderer/app/components/wallet/settings/SetWalletPassword';
import StoryDecorator from '../../_support/StoryDecorator';
import ChangeSpendingPasswordDialog from '../../../../source/renderer/app/components/wallet/settings/ChangeSpendingPasswordDialog';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../../_support/utils';
import STAKE_POOLS from '../../../../source/renderer/app/config/stakingStakePools.dummy';
import Wallet from '../../../../source/renderer/app/domains/Wallet';

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      uniqueId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};
const WALLETS = [
  generateWallet(
    'First Wallet',
    '1000000000',
    assets,
    0,
    STAKE_POOLS[0],
    false
  ),
];
const activeWallet: Wallet = WALLETS[0];
let walletPasswordIsOpen = false;
storiesOf('Wallets|Set Password', module)
  .addDecorator((story, context) => (
    <StoryDecorator>{withKnobs(story, context)}</StoryDecorator>
  ))
  .add('Set Wallet Password', () => (
    <>
      <SetWalletPassword
        isSetWalletPasswordDialogOpen={false}
        onSetWalletPassword={() => {
          walletPasswordIsOpen = !walletPasswordIsOpen;
        }}
      />
      {boolean('isSetWalletPasswordDialogOpen', false) && (
        <ChangeSpendingPasswordDialog
          isSpendingPasswordSet={false}
          currentPasswordValue={''}
          newPasswordValue={''}
          repeatedPasswordValue={''}
          onSave={() => {}}
          onCancel={() => {}}
          onDataChange={() => {}}
          isSubmitting={false}
          error={undefined}
          walletName={activeWallet.name}
          currentLocale={'en-US'}
        />
      )}
    </>
  ))
  .add('Change Wallet Password', () => (
    <ChangeSpendingPasswordDialog
      isSpendingPasswordSet
      currentPasswordValue={''}
      newPasswordValue={''}
      repeatedPasswordValue={''}
      onSave={() => {}}
      onCancel={() => {}}
      onDataChange={() => {}}
      isSubmitting={false}
      error={undefined}
      walletName={activeWallet.name}
      currentLocale={'en-US'}
    />
  ));
