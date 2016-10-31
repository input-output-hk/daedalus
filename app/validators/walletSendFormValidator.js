import MobxReactForm from 'mobx-react-form';
import WalletAddressValidator from 'wallet-address-validator';
import isCurrency from 'validator/lib/isCurrency';
import { defineMessages } from 'react-intl';
import { intl } from '../i18n-setup';

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
  description: {
  },
};

const options = {
  validateOnChange: false
};

class WalletSendForm extends MobxReactForm {

  onSuccess(form) {
    console.log('Form is valid! Send the request here.', this); // eslint-disable-line
    // get field values
    console.log('Form Values!', form.values()); // eslint-disable-line
  }

  onError(form) {
    // get all form errors
    console.log('All form errors', form.errors(), this); // eslint-disable-line
  }
}

export default new WalletSendForm({ options, fields });
