import { action } from 'mobx';
import moment from 'moment';
import store from '../store';

export const createWallet = action('createWallet', (props) => {
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
        date: new Date()
      },
      {
        id: 't-id-2',
        title: 'Money from Darko',
        type: 'adaExpend',
        amount: 500,
        currency: 'ADA',
        date: new Date()
      },
      {
        id: 't-id-3',
        title: 'Money to Dominik',
        type: 'adaIncome',
        amount: -400.58,
        currency: 'ADA',
        date: moment().subtract(1, 'days').toDate()
      },
      {
        id: 't-id-4',
        title: 'ADA to ETH',
        type: 'exchange',
        amount: -100000,
        currency: 'ADA',
        date: moment().subtract(1, 'days').toDate()
      },
    ]
  };
});
