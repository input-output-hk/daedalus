import { action } from 'mobx';
import store from '../store';

export const send = props => {
  // TODO: make real api request here instead of this stub
  setTimeout(action(() => {
    store.wallet.transactions.push({
      id: `t-id-${store.wallet.transactions.length + 1}`,
      title: `Money to ${props.receiver}`,
      type: 'adaExpend',
      amount: parseFloat(props.amount),
      currency: 'ADA',
      date: new Date()
    });

    store.router.transitionTo('/wallet/home');
  }), 500);
};
