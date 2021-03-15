// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { linkTo } from '@storybook/addon-links';
import { number } from '@storybook/addon-knobs';

import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../_support/utils';
// Screens
import UndelegateWalletConfirmationDialog from '../../../source/renderer/app/components/wallet/settings/UndelegateWalletConfirmationDialog';
import UndelegateWalletSuccessDialog from '../../../source/renderer/app/components/wallet/settings/UndelegateWalletSuccessDialog';

const assets = {
  available: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
  total: [
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
    {
      id: generateHash(),
      policyId: generatePolicyIdHash(),
      assetName: '',
      quantity: new BigNumber(200),
    },
  ],
};

const generalWallet = generateWallet(
  'Wallet 1',
  '1000000000',
  assets,
  0,
  STAKE_POOLS[0]
);
const hardwareWallet = generateWallet(
  'Wallet 1',
  '10000000',
  assets,
  0,
  STAKE_POOLS[0],
  false,
  'ready',
  true
);

export const StakingUndelegateConfirmationStory = (props: {
  unknownStakePool?: boolean,
  isHardwareWallet?: boolean,
}) => (
  <UndelegateWalletConfirmationDialog
    selectedWallet={props.isHardwareWallet ? hardwareWallet : generalWallet}
    stakePoolName={!props.unknownStakePool ? 'Lush 1' : null}
    stakePoolTicker={!props.unknownStakePool ? 'LSH1' : null}
    onConfirm={linkTo(
      'Decentralization | Staking',
      'Undelegate Confirmation Result'
    )}
    onCancel={() => null}
    onExternalLinkClick={() => null}
    isSubmitting={false}
    error={null}
    fees={{
      fee: new BigNumber(number('fee', 3)),
      deposits: new BigNumber(0),
      depositsReclaimed: new BigNumber(number('depositsReclaimed', 10)),
    }}
    hwDeviceStatus="ready"
  />
);

export const StakingUndelegateConfirmationResultStory = ({
  locale,
}: {
  locale: string,
}) => (
  <UndelegateWalletSuccessDialog
    walletName="Darko's ADA"
    slotLength={null}
    onClose={() => null}
    currentLocale={locale}
    futureEpochStartTime={moment().add(35, 'hour').toString()}
  />
);
