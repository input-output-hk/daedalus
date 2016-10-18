import MobxReactForm from 'mobx-react-form';
import WalletAddressValidator from 'wallet-address-validator';
import isCurrency from 'validator/lib/isCurrency';

const isBitcoinAddress = ({ field }) => {
  const isValid = WalletAddressValidator.validate(field.value, 'BTC');
  return [isValid, 'Please enter a valid Bitcoin address.'];
};

const isValidAmount = ({ field }) => {
  const isValid = isCurrency(field.value, {
    allow_negatives: false
  });
  return [isValid, 'Please enter a valid amount.'];
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
