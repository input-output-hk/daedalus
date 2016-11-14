// @flow
import sinon from 'sinon';

const sendMoneyFromMainWalletArguments = {
  sender: '13GvjwDkz8s8ZmGQjwVLNUXrNXdSmQa72x',
  receiver: '1s6GmpFXBmSE2Q15HPbKXdjgeJNGFhuwX',
  amount: 100,
  currency: 'ada',
  description: ''
};

const sendMoneyFromMainWalletTransactionData = {
  id: 't-id-5',
  type: 'adaExpend',
  title: 'Money to John',
  date: new Date(),
  transactionId: '6c310aac2fbdaa4890ce5c4e4f907aa62a51ab4ad102d9818aaf264add85f97a'
};

export const sendMoney = sinon.stub();

sendMoney.withArgs(sendMoneyFromMainWalletArguments).yieldsAsync(null, {
  ...sendMoneyFromMainWalletArguments,
  ...sendMoneyFromMainWalletTransactionData
});

sendMoney.yieldsAsync('Error sending money', null);
