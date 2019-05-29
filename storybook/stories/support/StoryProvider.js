// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { Provider, observer } from 'mobx-react';
import { observable, computed, runInAction } from 'mobx';
import BigNumber from 'bignumber.js';
import moment from 'moment';
import actions from '../../../source/renderer/app/actions';
import { WalletAssuranceModeOptions } from '../../../source/renderer/app/domains/Wallet';

type Props = {
  children: Node,
};

export const WALLETS = [
  {
    id: '0',
    name: 'No Password',
    amount: new BigNumber(66.998),
    assurance: WalletAssuranceModeOptions.NORMAL,
    hasPassword: false,
    passwordUpdateDate: new Date(),
    isLegacy: true,
  },
  {
    id: '1',
    name: 'With Password',
    amount: new BigNumber(0),
    assurance: WalletAssuranceModeOptions.NORMAL,
    hasPassword: true,
    passwordUpdateDate: moment()
      .subtract(1, 'month')
      .toDate(),
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
      ada: {
        wallets: {
          active: WALLETS[parseInt(this.activeWalletId, 10)],
          sendMoney: () => {},
          sendMoneyRequest: {
            isExecuting: false,
            reset: () => {},
          },
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
