// @flow
import data from '../data';

export const sendMoney = (request: {
  sender: string,
  receiver: string,
  amount: number,
  currency: string,
  description: ?string,
}) => new Promise((resolve) => {
  const walletTransactions = data.transactions[request.sender];
  const transaction = Object.assign({}, request, {
    id: `t-id-${walletTransactions.length}`,
    type: 'adaExpend',
    title: `Money to ${request.receiver}`,
    transactionId: 'ede9be2973a2558e2384cd09b135a1de7cc67ffd2cbb81be403f5e0b00400e1a',
    amount: -1 * request.amount
  });
  walletTransactions.push(transaction);
  resolve(transaction);
});
