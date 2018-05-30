import React, { Component } from 'react';
import { Provider } from 'mobx-react';
import { RouterStore, syncHistoryWithStore } from 'mobx-react-router';
import { hashHistory } from 'react-router';

import actions from '../../../source/renderer/app/actions';

type Props = {
  children: Node,
  activeWallet?: ?object
};

export default class StoryProvider extends Component<Props> {

  render() {

    const { activeWallet={}, children } = this.props;
    const router = new RouterStore();

    const stores = {
      ada: {
        wallets: {
          active: activeWallet,
          sendMoney: ()=>{},
          sendMoneyRequest: {
            isExecuting: false,
            reset: ()=>{}
          }
        }
      }
    };

    return (
      <Provider stores={stores} actions={actions}>
        {children}
      </Provider>
    );
  }

}
