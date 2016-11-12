import { action } from 'mobx';
import moment from 'moment';
import store from '../store';

export const createWallet = (props) => {
  // TODO: make real api request here instead of this stub
  setTimeout(action(() => {
    store.wallet = {
      name: props.walletName,
      address: '3ERitrYNwfxs4R6GfdULjtnTXQGCDE4iR7',
      currency: props.currency,
      amount: 19903750165.23,
      transactions: [
        {
          id: 't-id-1',
          title: 'MCDONALDS 9059098 Luna',
          type: 'card',
          amount: -502.40,
          currency: '$',
          date: new Date(),
          description: ''
        },
        {
          id: 't-id-2',
          title: 'Money to Darko',
          type: 'adaExpend',
          amount: 500,
          currency: 'ADA',
          date: new Date(),
          exchange: '502.40 ADA for 3.20 USD',
          conversionRate: '1 USD = 157 ADA',
          transactionId: '4f43b64ec9009ded75cc353f301d9f23a3c936d9b306af8fbb59f43e95244fe8',
          description: 'for invoice 1023'
        },
        {
          id: 't-id-3',
          title: 'Money from Dominik',
          type: 'adaIncome',
          amount: -400.58,
          currency: 'ADA',
          date: moment().subtract(1, 'days').toDate(),
          exchange: '502.40 ADA for 3.20 USD',
          conversionRate: '1 USD = 157 ADA',
          transactionId: '4f43b64ec9009ded75cc353f301d9f23a3c936d9b306af8fbb59f43e95244fe8',
          description: 'for invoice 1023'
        },
        {
          id: 't-id-4',
          title: 'ADA to ETH',
          type: 'exchange',
          amount: -100000,
          currency: 'ADA',
          date: moment().subtract(1, 'days').toDate(),
          exchange: '502.40 ADA for 1.25 ETH',
          conversionRate: '1 ETH = 401.92 ADA',
          transactionId: '4f43b64ec9009ded75cc353f301d9f23a3c936d9b306af8fbb59f43e95244fe8',
          description: ''
        },
      ]
    };
  }), 500);
};

export const send = (props) => {
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
