import React from 'react';
import { find } from 'lodash';
import BigNumber from 'bignumber.js';
import { number, boolean, select } from '@storybook/addon-knobs';
import { action } from '@storybook/addon-actions';
import StakePools from '../../../source/renderer/app/components/staking/stake-pools/StakePools';
import {
  CIRCULATING_SUPPLY,
  INITIAL_DESIRED_POOLS_NUMBER,
} from '../../../source/renderer/app/config/stakingConfig';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import {
  generateHash,
  generatePolicyIdHash,
  generateWallet,
} from '../_support/utils';

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
const dummyWallets = [
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
  generateWallet('Dummy1', '1000000000000', assets, 0, STAKE_POOLS[0]),
  generateWallet(
    'Dummy2',
    '2000000000000',
    assets,
    0,
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
    STAKE_POOLS[1],
    true,
    'syncing'
  ),
  generateWallet('Dummy3', '2000000000000', assets),
];
const maxDelegationFunds = Math.round(
  CIRCULATING_SUPPLY / INITIAL_DESIRED_POOLS_NUMBER
);
type Props = {
  currentTheme: string;
  locale: string;
  isLoading: boolean;
};
export const StakePoolsStory = (props: Props) => {
  const selectedWallet = select(
    'selectedWallet',
    {
      'All wallets': '0',
      ...dummyWallets.reduce((obj, wallet) => {
        obj[wallet.name] = wallet.id;
        return obj;
      }, {}),
    },
    null
  );
  return (
    // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
    <StakePools
      stakePoolsList={STAKE_POOLS.slice(
        0,
        number('Pools', 300, {
          range: true,
          min: 37,
          max: 300,
          step: 1,
        })
      )}
      stakePoolsDelegatingList={[
        STAKE_POOLS[1],
        STAKE_POOLS[3],
        STAKE_POOLS[20],
        STAKE_POOLS[36],
      ]}
      isFetching={boolean('isFetching', false)}
      onOpenExternalLink={action('onOpenExternalLink')}
      currentTheme={props.currentTheme}
      currentLocale={props.locale}
      onDelegate={action('onDelegate')}
      isLoading={props.isLoading}
      isRanking={false}
      updateDelegatingStake={() => null}
      rankStakePools={() => null}
      wallets={dummyWallets}
      getStakePoolById={(poolId) =>
        find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
      }
      onSmashSettingsClick={action('onSmashSettingsClick')}
      smashServerUrl="https://smash.cardano-mainnet.iohk.io"
      maxDelegationFunds={maxDelegationFunds}
      selectedDelegationWalletId={selectedWallet}
    />
  );
};
