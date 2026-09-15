import React from 'react';
import { storiesOf } from '@storybook/react';
import { withKnobs, boolean, text } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import BigNumber from 'bignumber.js';
// Helpers
import StoryDecorator from '../../_support/StoryDecorator';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../../_support/utils';
import STAKE_POOLS from '../../../../source/renderer/app/config/stakingStakePools.dummy.json';
// Screens
import UndelegateWalletConfirmationDialog from '../../../../source/renderer/app/components/wallet/settings/UndelegateWalletConfirmationDialog';

const undelegateWalletId = 'Undelegate Wallet';
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
const selectedWallet = generateWallet(
  'Wallet 1',
  '1000000000',
  assets,
  0,
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
  STAKE_POOLS[0]
);

storiesOf('Wallets / Settings', module)
  .addDecorator(withKnobs)
  .addDecorator((story) => <StoryDecorator>{story()}</StoryDecorator>) // ====== Stories ======
  .add('Undelegate Wallet', () => (
    <UndelegateWalletConfirmationDialog
      selectedWallet={selectedWallet}
      stakePoolName={text(
        'UndelegateWalletConfirmationDialog: Stake Pool Name',
        'Stake Pool Name'
      )}
      stakePoolTicker={text(
        'UndelegateWalletConfirmationDialog: Stake Pool Ticker',
        'Stake Pool Ticker'
      )}
      onConfirm={action('Undelegate Wallet - onConfirm')}
      onCancel={action('Undelegate Wallet - onCancel')}
      onExternalLinkClick={action('Undelegate Wallet - onExternalLinkClick')}
      isSubmitting={boolean(
        'Undelegate Wallet - submitting',
        false,
        undelegateWalletId
      )}
      error={null}
      // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
      fees={new BigNumber(10)}
      hwDeviceStatus="ready"
      isTrezor={boolean('isTrezor', false)}
    />
  ));
