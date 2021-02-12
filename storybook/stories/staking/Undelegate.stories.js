// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { linkTo } from '@storybook/addon-links';

import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { generateWallet } from '../_support/utils';
// Screens
import UndelegateWalletConfirmationDialog from '../../../source/renderer/app/components/wallet/settings/UndelegateWalletConfirmationDialog';
import UndelegateWalletSuccessDialog from '../../../source/renderer/app/components/wallet/settings/UndelegateWalletSuccessDialog';

const selectedWallet = generateWallet(
  'Wallet 1',
  '1000000000',
  0,
  STAKE_POOLS[0]
);

export const StakingUndelegateConfirmationStory = (props: {
  unknownStakePool?: boolean,
}) => (
  <UndelegateWalletConfirmationDialog
    selectedWallet={selectedWallet}
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
    fees={new BigNumber(33333.33)}
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
