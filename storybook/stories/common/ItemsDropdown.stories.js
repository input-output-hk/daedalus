// @flow
import React from 'react';
import { storiesOf } from '@storybook/react';
import { observable, action as mobxAction } from 'mobx';
import { action } from '@storybook/addon-actions';
import {
  withKnobs,
  text,
  boolean,
  number,
  select,
} from '@storybook/addon-knobs';
import { escapeRegExp, find } from 'lodash';
import StoryDecorator from '../_support/StoryDecorator';
import StoryProvider from '../_support/StoryProvider';
import StoryLayout from '../_support/StoryLayout';
import ItemsDropdown from '../../../source/renderer/app/components/widgets/forms/ItemsDropdown';
import WalletsDropdown from '../../../source/renderer/app/components/widgets/forms/WalletsDropdown';
import WalletsDropdownOption from '../../../source/renderer/app/components/widgets/forms/WalletsDropdownOption';
import WalletsDropdownTopLabel from '../../../source/renderer/app/components/widgets/forms/WalletsDropdownTopLabel';
import STAKE_POOLS from '../../../source/renderer/app/config/stakingStakePools.dummy.json';
import { generateWallet } from '../_support/utils';

const WALLETS = [
  generateWallet('Second Wallet', '500000000'),
  generateWallet('Third Wallet', '100000000'),
  generateWallet('Fourth Wallet', '50000000'),
  generateWallet('Fifth Wallet', '7000000'),
];

const stakePoolsList = [
  ...STAKE_POOLS.slice(0, 5),
  ...STAKE_POOLS.slice(150, 155),
  ...STAKE_POOLS.slice(290, 295),
];

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
            <div style={{ margin: 50, height: 400 }}>{story()}</div>
          </StoryLayout>
        </StoryProvider>
      </StoryDecorator>
    );
  })

  .addDecorator(withKnobs)

  // ====== Stories ======

  .add('Generic', () => {
    const options = [
      {
        topLabel: 'Item 1 - top',
        bottomLabel: 'Item 1 - bottom',
        value: 'item1',
        label: 'LABEL',
      },
      {
        topLabel: 'Item 2 - top',
        bottomLabel: 'Item 2 - bottom',
        value: 'item2',
      },
      {
        topLabel: 'Item 3 - top',
        bottomLabel: 'Item 3 - bottom',
        value: 'item3',
      },
    ];
    const errorPosition = select(
      'Error',
      {
        'No error': null,
        'Top error': 'top',
        'Bottom error': 'bottom',
      },
      null
    );
    return (
      <ItemsDropdown
        options={options}
        onChange={action('onChange')}
        value={'item2'}
        onSearch={(searchValue) => {
          return options.filter((option) => {
            const { topLabel, bottomLabel, value } = option;
            const regex = new RegExp(escapeRegExp(searchValue), 'i');
            return (
              regex.test(topLabel) ||
              regex.test(bottomLabel) ||
              regex.test(value)
            );
          });
        }}
        errorPosition={errorPosition}
        error={boolean('Has error', false) ? 'Error message' : ''}
      />
    );
  })

  .add('Wallets - Standalone option', () => {
    return (
      <WalletsDropdownOption
        delegatedStakePool={
          boolean('has delegatedStakePool') ? STAKE_POOLS[0] : null
        }
        detail={text('detail', 'detail')}
        label={text('label', 'label')}
        numberOfStakePools={number('numberOfStakePools', 1)}
        selected={boolean('selected', false)}
        isSyncing={boolean('isSyncing', false)}
        syncingLabel={text('syncingLabel')}
        isHardwareWallet={boolean('isHardwareWallet', false)}
      />
    );
  })

  .add('Wallets - TopLabel', () => {
    const wallet = generateWallet(
      text('Wallet - Name', 'Wallet name'),
      `${number('Wallet - Amount', 1000000000)}`,
      undefined,
      undefined,
      select('Wallet - Stake pool', stakePoolsOptions, STAKE_POOLS[0]),
      true,
      undefined,
      boolean('Wallet - isHardwareWallet', false)
    );
    return (
      <WalletsDropdownTopLabel
        getStakePoolById={(poolId) =>
          find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
        }
        isSyncing={boolean('isSyncing', false)}
        numberOfStakePools={
          boolean('Has stake pools', false) ? STAKE_POOLS.length : 0
        }
        syncingLabel={text('syncingLabel', 'Syncing')}
        wallet={wallet}
      />
    );
  })

  .add('Wallets', () => {
    const firstWallet = generateWallet(
      text('Name', 'First Wallet', 'First wallet'),
      `${number('Amount', 1000000000, {}, 'First wallet')}`,
      [],
      undefined,
      select('Stake pool', stakePoolsOptions, STAKE_POOLS[0]),
      true,
      undefined,
      boolean('Wallet - isHardwareWallet', true, 'First wallet')
    );
    const wallets = [firstWallet, ...WALLETS.slice(1)];
    return (
      <WalletsDropdown
        getStakePoolById={(poolId) =>
          find(STAKE_POOLS, (stakePool) => stakePool.id === poolId)
        }
        isSyncing={boolean('isSyncing', false)}
        label={text('label', 'Wallets')}
        numberOfStakePools={STAKE_POOLS.length}
        onChange={action('onChange')}
        placeholder={text('placeholder')}
        syncingLabel={text('syncingLabel', 'Syncing')}
        value={firstWallet.id}
        wallets={wallets}
      />
    );
  });
