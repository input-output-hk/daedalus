import MobxReactForm from 'mobx-react-form';
import { defineMessages } from 'react-intl';

const messages = defineMessages({
  invalidWalletName: {
    id: 'wallet.create.form.errors.invalidWalletName',
    defaultMessage: '!!!The wallet name must have at least 3 letters.',
    description: 'Error message shown when invalid wallet name was entered.'
  },
});

const isValidWalletName = ({ field }) => {
  const isValid = field.value.length >= 3;
  return [isValid, messages.invalidWalletName];
};

const fields = {
  walletName: {
    validate: [isValidWalletName]
  },
  currency: {
  },
};

const options = {
  validateOnChange: false
};

class WalletCreateForm extends MobxReactForm {

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

export default new WalletCreateForm({ options, fields });
