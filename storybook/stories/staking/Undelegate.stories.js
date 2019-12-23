// @flow
import React from 'react';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import { linkTo } from '@storybook/addon-links';

// Screens
import UndelegateConfirmationDialog from '../../../source/renderer/app/components/staking/delegation-center/UndelegateConfirmationDialog';
import UndelegateConfirmationResultDialog from '../../../source/renderer/app/components/staking/delegation-center/UndelegateConfirmationResultDialog';

export const StakingUndelegateConfirmationStory = (props: {
  unknownStakePool?: boolean,
}) => (
  <UndelegateConfirmationDialog
    walletName="Darko's ADA"
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
  />
);

export const StakingUndelegateConfirmationResultStory = ({
  locale,
}: {
  locale: string,
}) => (
  <UndelegateConfirmationResultDialog
    walletName="Darko's ADA"
    onClose={() => null}
    currentLocale={locale}
    futureEpochStartTime={moment()
      .add(35, 'hour')
      .toString()}
  />
);
