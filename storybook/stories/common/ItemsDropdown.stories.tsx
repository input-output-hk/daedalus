import React from 'react';
import { storiesOf } from '@storybook/react';
import { observable, action as mobxAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import { withState } from '@dump247/storybook-state';
import {
  withKnobs,
  text,
  boolean,
  number,
  select,
} from '@storybook/addon-knobs';
import { find, get } from 'lodash';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import StoryLayout from '../_support/StoryLayout';
import ItemsDropdown from '../../../source/renderer/app/components/widgets/forms/ItemsDropdown';
import WalletsDropdown from '../../../source/renderer/app/components/widgets/forms/WalletsDropdown';
import WalletsDropdownLabel from '../../../source/renderer/app/components/widgets/forms/WalletsDropdownLabel';
import AssetsDropdown from '../../../source/renderer/app/components/widgets/forms/AssetsDropdown';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import currenciesList from '../../../source/renderer/app/config/currenciesList.json';
import {
  generateWallet,
  generateHash,
  generateAssetToken,
} from '../_support/utils';
import { WalletSyncStateStatuses } from '../../../source/renderer/app/domains/Wallet';

const WALLETS = [
  generateWallet('Second Wallet', '500000000'),
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
  generateWallet('Third Wallet', '100000000', STAKE_POOLS[3]),
  generateWallet(
    'Fourth Syncing Wallet',
    '50000000',
    undefined,
    undefined,
    undefined,
    true,
    WalletSyncStateStatuses.SYNCING
  ),
  generateWallet('Fifth Wallet', '7000000'),
];
const stakePoolsList = [
  ...STAKE_POOLS.slice(0, 5),
  ...STAKE_POOLS.slice(150, 155),
  ...STAKE_POOLS.slice(290, 295),
];
const assets = [
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  generateAssetToken(generateHash(), '', generateHash(), 100, {
    name: 'Asset 1',
    ticker: 'ABCD',
    description: 'Asset 1 description',
  }),
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  generateAssetToken(generateHash(), '', generateHash(), 200, {
    name: 'Asset 2',
    ticker: 'EFG',
    description: 'Asset 2 description',
  }),
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  generateAssetToken(generateHash(), '', generateHash(), 300, {
    name: 'Asset 3',
    ticker: 'HI',
    description: 'Asset 3 description',
  }),
  // @ts-ignore ts-migrate(2554) FIXME: Expected 7 arguments, but got 5.
  generateAssetToken(generateHash(), '', generateHash(), 400, {
    name: 'Asset 4',
    ticker: 'JKL',
    description: 'Asset 4 description',
  }),
];
const firstWalletId = generateHash();
const stakePoolsOptions = stakePoolsList.reduce((obj, pool) => {
  const { name, ticker, ranking } = pool;
  obj[`[${ticker}] ${name} - (${ranking})`] = pool;
  return obj;
}, {});
storiesOf('Common|ItemsDropdown', module)
  .addDecorator((story: any, context: any) => {
    if (context.name === 'CountdownWidget') {
      return story();
    }

    const onChangeAction = action('onChange');
    const state = observable({
      checked: false,
      onChange: mobxAction((value, event) => {
        state.checked = value;
        onChangeAction(value, event);
      }),
    });
    return (
      <StoryDecorator propsForChildren={state}>
        <StoryProvider>
          <StoryLayout activeSidebarCategory={null} {...context}>
            <div
              style={{
                margin: 50,
              }}
            >
              {story()}
            </div>
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })
  .addDecorator(withKnobs) // ====== Stories ======
  .add(
    'Generic',
    withState(
      {
        value: 'usd',
      },
      (store) => {
        const options = Object.values(currenciesList).map((currency, index) => {
          const label = get(currency, 'name.en-US');
          const code = get(currency, 'code');
          const decimalDigits = get(currency, 'decimalDigits');
          const detail = `Code: ${code} - Decimal digits: ${decimalDigits}`;
          const value = code;
          const isSyncing = index === 1;
          return {
            label,
            detail,
            value,
            isSyncing,
          };
        });
        return (
          <ItemsDropdown
            options={options}
            // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
            value={store.state.value}
            onChange={(value) =>
              store.set({
                value,
              })
            }
            hasSearch
            error={boolean('Has error', false) ? 'Error message' : ''}
          />
        );
      }
    )
  )
  .add(
    'Wallets',
    withState(
      {
        walletId: firstWalletId,
      },
      (store) => {
        const firstWallet = generateWallet(
          text('Name', 'First Wallet', 'First wallet'),
          `${number('Amount', 1000000000, {}, 'First wallet')}`,
          undefined,
          undefined,
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'SelectTypeKnobValue' is not assi... Remove this comment to see the full error message
          select(
            'Stake pool',
            stakePoolsOptions,
            // @ts-ignore ts-migrate(2345) FIXME: Argument of type '{ relativeStake: number; cost: s... Remove this comment to see the full error message
            STAKE_POOLS[0],
            'First wallet'
          ),
          true,
          boolean('isSyncing', false, 'First wallet')
            ? WalletSyncStateStatuses.SYNCING
            : WalletSyncStateStatuses.READY,
          boolean('Wallet - isHardwareWallet', true, 'First wallet'),
          firstWalletId
        );
        const wallets = [firstWallet, ...WALLETS];
        return (
          <WalletsDropdown
            getStakePoolById={(poolId) =>
              find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
            }
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ getStakePoolById: (poolId: any) => { relat... Remove this comment to see the full error message
            label={text('label', 'Wallets')}
            numberOfStakePools={
              boolean('Has stake pools', true, 'First wallet')
                ? STAKE_POOLS.length
                : 0
            }
            onChange={(walletId) =>
              store.set({
                walletId,
              })
            }
            // @ts-ignore ts-migrate(2554) FIXME: Expected 2-3 arguments, but got 1.
            placeholder={text('placeholder')}
            syncingLabel={text('syncingLabel', 'syncing')}
            value={store.state.walletId}
            wallets={wallets}
            hasSearch={boolean('hasSearch', false)}
          />
        );
      }
    )
  )
  .add('Wallets - Label only', () => {
    const wallet = generateWallet(
      text('Wallet - Name', 'Wallet name'),
      '1000000000',
      undefined,
      undefined,
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'SelectTypeKnobValue' is not assi... Remove this comment to see the full error message
      select('Wallet - Stake pool', stakePoolsOptions, STAKE_POOLS[0]),
      true,
      undefined,
      boolean('Wallet - isHardwareWallet', true)
    );
    return (
      <div
        style={{
          fontFamily: 'var(--font-regular)',
          fontSize: 14,
          lineHeight: 18,
          color:
            'var(--theme-delegation-steps-choose-wallet-custom-value-color)',
        }}
      >
        <WalletsDropdownLabel
          getStakePoolById={(poolId) =>
            find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
          }
          // @ts-ignore ts-migrate(2769) FIXME: No overload matches this call.
          isSyncing={boolean('isSyncing', false)}
          numberOfStakePools={
            boolean('Has stake pools', true) ? STAKE_POOLS.length : 0
          }
          syncingLabel={text('syncingLabel', 'syncing')}
          wallet={wallet}
        />
      </div>
    );
  })
  .add(
    'Assets',
    withState(
      {
        assetId: assets[0].fingerprint,
      },
      (store) => {
        return (
          <AssetsDropdown
            assets={assets}
            // @ts-ignore ts-migrate(2322) FIXME: Type '{ assets: AssetToken[]; value: string; onCha... Remove this comment to see the full error message
            value={store.state.assetId}
            onChange={(assetId) =>
              store.set({
                assetId,
              })
            }
          />
        );
      }
    )
  );
