import React, { Component } from 'react';
import { Provider, observer } from 'mobx-react';
import { RouterStore, syncHistoryWithStore } from 'mobx-react-router';
import { hashHistory } from 'react-router';

import actions from '../../../source/renderer/app/actions';

type Props = {
  children: Node,
};

export default class StoreContainer extends Component<Props> {

  render() {

    const router = new RouterStore();

    const stores = {
      ada: {
        wallets: {
          active: {},
          sendMoney: ()=>{},
          sendMoneyRequest: {
            isExecuting: false,
            reset: ()=>{}
          }
        }
      }
    };

    const { children } = this.props;

    return (
      <Provider stores={stores} actions={actions}>
        {children}
      </Provider>
    );
  }

}
