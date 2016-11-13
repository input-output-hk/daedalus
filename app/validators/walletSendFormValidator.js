import MobxReactForm from 'mobx-react-form';
import WalletAddressValidator from 'wallet-address-validator';
import isCurrency from 'validator/lib/isCurrency';
import { defineMessages } from 'react-intl';
import { intl } from '../i18n';
import { sendMoney } from '../actions/wallet-actions';

const messages = defineMessages({
  invalidBitcoinAddress: {
    id: 'wallet.send.form.errors.invalidBitcoinAddress',
    defaultMessage: '!!!Please enter a valid Bitcoin address.',
    description: 'Error message shown when invalid bitcoin address was entered.'
  },
  invalidAmount: {
    id: 'wallet.send.form.errors.invalidAmount',
    defaultMessage: '!!!Please enter a valid amount.',
    description: 'Error message shown when invalid amount was entered.'
  }
});

const isBitcoinAddress = ({ field }) => {
  const isValid = WalletAddressValidator.validate(field.value, 'BTC');
  return [isValid, intl.formatMessage(messages.invalidBitcoinAddress)];
};

const isValidAmount = ({ field }) => {
  const isValid = isCurrency(field.value, {
    allow_negatives: false
  });
  return [isValid, intl.formatMessage(messages.invalidAmount)];
};

const fields = {
  receiver: {
    validate: [isBitcoinAddress]
  },
  amount: {
    validate: [isValidAmount]
  },
  currency: {
    value: 'ada' // TODO: Remove hardcoded currency after new version of send screen is implemented
  },
  description: {
  },
};

const options = {
  validateOnChange: false
};

class WalletSendForm extends MobxReactForm {

  onSuccess(form) {
    sendMoney(form.values());
    form.reset();
  }

  onError(form) {
    // get all form errors
    console.log('All form errors', form.errors(), this); // eslint-disable-line
  }
}

export default new WalletSendForm({ options, fields });
