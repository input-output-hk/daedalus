// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { Provider, observer } from 'mobx-react';
import { observable, computed, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import actions from '../../../source/renderer/app/actions';

type Props = {
  children: Node,
};

export const WALLETS = [
  {
    id: '0',
    name: 'No Password',
    amount: new BigNumber(66.998),
    hasPassword: false,
    passwordUpdateDate: new Date(),
    syncState: { data: null, tag: 'synced' },
    isLegacy: false,
  },
  {
    id: '1',
    name: 'With Password',
    amount: new BigNumber(0),
    hasPassword: true,
    passwordUpdateDate: moment()
      .subtract(1, 'month')
      .toDate(),
    syncState: { data: null, tag: 'synced' },
    isLegacy: false,
  },
  {
    id: '2',
    name: 'Legacy',
    amount: new BigNumber(55.555),
    hasPassword: false,
    passwordUpdateDate: new Date(),
    syncState: { data: null, tag: 'synced' },
    isLegacy: true,
  },
  {
    id: '3',
    name: 'Restoring',
    amount: new BigNumber(12.345),
    hasPassword: false,
    passwordUpdateDate: new Date(),
    syncState: {
      data: {
        estimatedCompletionTime: {
          quantity: 123456789,
          unit: 'milliseconds',
        },
        percentage: {
          quantity: 50,
          unit: 'percent',
        },
        throughput: {
          quantity: 500,
          unit: 'blocksPerSecond',
        },
      },
      tag: 'restoring',
    },
    isLegacy: false,
  },
];

@observer
export default class StoryProvider extends Component<Props> {
  @observable activeWalletId = '0';

  @computed get storiesProps(): {} {
    return {
      wallets: WALLETS,
      activeWalletId: this.activeWalletId,
      setActiveWalletId: this.setActiveWalletId,
    };
  }

  @computed get stores(): {} {
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
        {this.props.children}
      </Provider>
    );
  }
}
