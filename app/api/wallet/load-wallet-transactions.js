// @flow
import moment from 'moment';

const transactions = {
  '13GvjwDkz8s8ZmGQjwVLNUXrNXdSmQa72x': [
    {
      id: 't-id-1',
      title: 'McDonalds 9059098 Luna',
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
      currency: 'ada',
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
      currency: 'ada',
      date: moment().subtract(1, 'days').toDate(),
      exchange: '400.58 ADA for 2.55 USD',
      conversionRate: '1 USD = 157 ADA',
      transactionId: '5fd924625f6ab16a19cc9807c7c506ae1813490e4ba675f843d5a10e0baacdb8',
      description: 'for invoice 1023'
    }
  ],
  '1GrT8upXQCR4mzPpAeMfxqwZh2LF7PXsyn': [
    {
      id: 't-id-1',
      title: 'KaDeWe Berlin',
      type: 'card',
      amount: -1764.20,
      currency: '$',
      date: new Date(),
      description: ''
    },
    {
      id: 't-id-2',
      title: 'Money from John',
      type: 'adaIncome',
      amount: 200,
      currency: 'ada',
      date: moment().subtract(1, 'days').toDate(),
      transactionId: 'a6dbc3f77ee7cc4248e6ed030e2e33936125f025d093c2f955cceb63353f05e8',
      description: 'for invoice 1023'
    },
    {
      id: 't-id-3',
      title: 'Money to John',
      type: 'adaExpend',
      amount: 1500,
      currency: 'ada',
      date: new Date(),
      exchange: '1500.00 ADA for 9.55 USD',
      conversionRate: '1 USD = 157 ADA',
      transactionId: '02a49c31e73a0801b6065b9d80c3dcbf95aff9bbb672ad73f8f3a79e3ac991c2',
      description: 'for invoice 1203'
    }
  ],
  '1LYYdFD4RGLsYJT5tVQoLQEvfr7hJXHRuv': [
    {
      id: 't-id-1',
      title: 'Apple store',
      type: 'card',
      amount: -5324,
      currency: '$',
      date: new Date(),
      description: ''
    },
    {
      id: 't-id-2',
      title: 'ADA to ETH',
      type: 'exchange',
      amount: -1000,
      currency: 'ada',
      date: moment().subtract(2, 'days').toDate(),
      exchange: '1000.00 ADA for 2.49 ETH',
      conversionRate: '1 ETH = 401.92 ADA',
      transactionId: '05fdcc578d8b593a98715fa7c6ce67e98058ced85ae48730275a55628aeac035',
      description: ''
    }
  ],
  '1MKY9oAriyXvUKRBRsbrrT2JDHA8S9T2sf': [
    {
      id: 't-id-1',
      title: 'Oyster Card Topup',
      type: 'card',
      amount: -20,
      currency: '£',
      date: new Date(),
      description: ''
    }
  ],
  fd17fda6410ac905447840e33c1851a5: [
    {
      id: 't-id-1',
      title: 'ADA to BTC',
      type: 'exchange',
      amount: 1000,
      currency: 'ada',
      date: moment().subtract(2, 'days').toDate(),
      exchange: '1000.00 ADA for 10.00 BTC',
      conversionRate: '1 BTC = 100.00 ADA',
      transactionId: '70f0fafd7435246853560ec765d7739ba6b7908f46bcc6d7258d86a863659838',
      description: ''
    }
  ]
};

export const loadWalletTransactions = (data: {
  address: string
}) => new Promise((resolve) => {
  resolve(transactions[data.address] ? transactions[data.address] : []);
});
