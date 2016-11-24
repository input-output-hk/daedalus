// @flow

let idCount = 4;

export const sendMoney = (transaction: {
  sender: string,
  receiver: string,
  amount: number,
  currency: string,
  description: ?string,
}) => new Promise((resolve) => {
  idCount += 1;
  resolve(Object.assign({}, transaction, {
    id: `t-id-${idCount}`,
    type: 'adaExpend',
    title: `Money to ${transaction.receiver}`,
    transactionId: 'ede9be2973a2558e2384cd09b135a1de7cc67ffd2cbb81be403f5e0b00400e1a',
  }));
});
