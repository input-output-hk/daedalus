// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import { linkTo } from '@storybook/addon-links';

// Screens
import UndelegateConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-center/UndelegateConfirmationDialog';
import UndelegateConfirmationResultDialog from '../../../source/renderer/app/components/staking/delegation-center/UndelegateConfirmationResultDialog';

export const StakingUndelegateConfirmationStory = () => (
  <UndelegateConfirmationDialog
    walletName="Darko's ADA"
    stakePoolName="Lush 1"
    stakePoolSlug="LSH1"
    onConfirm={linkTo(
      'Decentralization | Staking',
      'Undelegate Confirmation Result'
    )}
    onCancel={() => null}
    onExternalLinkClick={() => null}
    isSubmitting={false}
    error={null}
    fees={new BigNumber(33333.33)}
  />
);

export const StakingUndelegateConfirmationResultStory = () => (
  <UndelegateConfirmationResultDialog
    walletName="Darko's ADA"
    onClose={() => null}
  />
);
