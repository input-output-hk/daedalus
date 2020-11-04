// @flow
import moment from 'moment';

const isOdd = (number: number) => number % 2;

const date = moment().subtract('month', 1);
const getAddress = (txIndex: number, inputOutputIndex: number) =>
  `addr1qxwt274ux0n46rvg27k6dcepdewvh28ex0uua2u2gsuus7wp7n5ea7ryx48h2txjkf09cljahkwyvpawpm3ga6s8hynscqps${txIndex}${inputOutputIndex}`;
const amountBase = Math.round(Math.random() * 10) + 1000000000;

const getDummyTransaction = (index: number) => {
  const amount = amountBase + index;
  const direction = isOdd(index) ? 'incoming' : 'outgoing';
  if (isOdd(index)) date.add(1, 'day');
  const id = index;
  return {
    id,
    amount: {
      quantity: amount,
      unit: 'lovelace',
    },
    inserted_at: {
      time: date.toDate(),
      block: {
        slot_number: amountBase,
        epoch_number: amountBase,
      },
    },
    depth: {
      quantity: 10,
      unit: 'block',
    },
    direction,
    inputs: [
      {
        address: getAddress(index, 1),
        amount: {
          quantity: amount,
          unit: 'lovelace',
        },
        id,
        index: 1,
      },
    ],
    outputs: [
      {
        address: getAddress(index, 2),
        amount: {
          quantity: amount,
          unit: 'lovelace',
        },
        id,
        index: 1,
      },
    ],
    withdrawals: [],
    status: 'in_ledger',
  };
};

export const getDummyTransactionHistory = (numberOfTx: number = 30) =>
  [...Array(numberOfTx)].map((x, index) => getDummyTransaction(index));
