import React, { Component } from 'react';
// @ts-ignore ts-migrate(2305) FIXME: Module '"react"' has no exported member 'Node'.
import type { Node } from 'react';
import { Provider, observer } from 'mobx-react';
import faker from 'faker';
import { observable, computed, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import actions from '../../../source/renderer/app/actions';
import { WalletSyncStateStatuses } from '../../../source/renderer/app/domains/Wallet';
import {
  DiscreetModeFeatureProvider,
  BrowserLocalStorageBridge,
} from '../../../source/renderer/app/features';
import { DiscreetModeToggleKnob } from './DiscreetModeToggleKnob';
import { DiscreetModeNotificationKnob } from './DiscreetModeNotificationKnob';

type Props = {
  children: Node;
};
export const WALLETS = [
  {
    id: '0',
    name: 'With Password',
    amount: new BigNumber(0),
    reward: new BigNumber(0),
    hasPassword: true,
    passwordUpdateDate: moment().subtract(1, 'month').toDate(),
    syncState: {
      status: WalletSyncStateStatuses.READY,
    },
    isNotResponding: false,
    isRestoring: false,
    isLegacy: false,
    recoveryPhraseVerificationDate: new Date(),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '1',
    name: 'No Password',
    amount: new BigNumber(66.998),
    reward: new BigNumber(0),
    hasPassword: false,
    passwordUpdateDate: new Date(),
    syncState: {
      status: WalletSyncStateStatuses.READY,
    },
    isNotResponding: false,
    isRestoring: false,
    isLegacy: false,
    recoveryPhraseVerificationDate: new Date(),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '2',
    name: 'Legacy with funds',
    amount: new BigNumber(55.555),
    reward: new BigNumber(0),
    hasPassword: true,
    passwordUpdateDate: moment().subtract(1, 'month').toDate(),
    syncState: {
      status: WalletSyncStateStatuses.READY,
    },
    isNotResponding: false,
    isRestoring: false,
    isLegacy: true,
    recoveryPhraseVerificationDate: moment().subtract(200, 'days').toDate(),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '3',
    name: 'Legacy with no funds',
    amount: new BigNumber(0),
    reward: new BigNumber(0),
    hasPassword: true,
    passwordUpdateDate: moment().subtract(1, 'month').toDate(),
    syncState: {
      status: WalletSyncStateStatuses.READY,
    },
    isNotResponding: false,
    isRestoring: false,
    isLegacy: true,
    recoveryPhraseVerificationDate: moment().subtract(200, 'days').toDate(),
  },
  {
    id: '4',
    name: 'Restoring',
    amount: new BigNumber(12.345),
    reward: new BigNumber(0),
    hasPassword: true,
    passwordUpdateDate: moment().subtract(1, 'month').toDate(),
    syncState: {
      progress: {
        quantity: 50,
        unit: 'percent',
      },
      status: WalletSyncStateStatuses.RESTORING,
    },
    isNotResponding: false,
    isRestoring: true,
    isLegacy: false,
    recoveryPhraseVerificationDate: moment().subtract(400, 'days').toDate(),
  },
  {
    id: '5',
    name: 'Not responding',
    amount: new BigNumber(66.998),
    reward: new BigNumber(0),
    hasPassword: true,
    passwordUpdateDate: moment().subtract(1, 'month').toDate(),
    syncState: {
      status: WalletSyncStateStatuses.NOT_RESPONDING,
    },
    isNotResponding: true,
    isRestoring: false,
    isLegacy: false,
    recoveryPhraseVerificationDate: new Date(),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
];
export const WALLETS_V2 = [
  {
    id: '1',
    name: 'Wallet 1',
    amount: new BigNumber(faker.finance.amount()),
    reward: new BigNumber(faker.finance.amount()),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '2',
    name: 'Wallet 2',
    amount: new BigNumber(faker.finance.amount()),
    reward: new BigNumber(faker.finance.amount()),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '3',
    name: 'Wallet 3',
    amount: new BigNumber(faker.finance.amount()),
    reward: new BigNumber(faker.finance.amount()),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '4',
    name: 'Wallet 4',
    amount: new BigNumber(faker.finance.amount()),
    reward: new BigNumber(faker.finance.amount()),
    delegatedStakePoolId: 'kfhdsdkhfskdjfhskdhf',
  },
  {
    id: '5',
    name: 'Wallet 5',
    amount: new BigNumber(faker.finance.amount()),
    reward: new BigNumber(faker.finance.amount()),
  },
];

@observer
class StoryProvider extends Component<Props> {
  @observable
  activeWalletId = '0';

  @computed
  get storiesProps(): {} {
    return {
      wallets: WALLETS,
      activeWalletId: this.activeWalletId,
      setActiveWalletId: this.setActiveWalletId,
    };
  }

  @computed
  get stores(): {} {
    return {
      wallets: {
        active: WALLETS[parseInt(this.activeWalletId, 10)],
        sendMoney: () => {},
        sendMoneyRequest: {
          isExecuting: false,
          reset: () => {},
        },
      },
    };
  }

  setActiveWalletId = (walletId: string) =>
    runInAction(() => {
      this.activeWalletId = walletId;
    });

  render() {
    return (
      <Provider
        stores={this.stores}
        actions={actions}
        storiesProps={this.storiesProps}
      >
        <BrowserLocalStorageBridge>
          <DiscreetModeFeatureProvider>
            <>
              {this.props.children}
              <DiscreetModeToggleKnob />
              <DiscreetModeNotificationKnob />
            </>
          </DiscreetModeFeatureProvider>
        </BrowserLocalStorageBridge>
      </Provider>
    );
  }
}

export default StoryProvider;
