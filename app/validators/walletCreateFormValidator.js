import MobxReactForm from 'mobx-react-form';
import { defineMessages } from 'react-intl';
import state from '../state';
import { intl } from '../i18n';

const messages = defineMessages({
  invalidWalletName: {
    id: 'wallet.create.form.errors.invalidWalletName',
    defaultMessage: '!!!The wallet name must have at least 3 letters.',
    description: 'Error message shown when invalid wallet name was entered in create wallet dialog.'
  },
  invalidCurrency: {
    id: 'wallet.create.form.errors.invalidCurrency',
    defaultMessage: '!!!This currency is not yet supported.',
    description: 'Error message shown when invalid currency was selected in create wallet dialog.'
  },
});

const isValidWalletName = ({ field }) => {
  const isValid = field.value.length >= 3;
  return [isValid, intl.formatMessage(messages.invalidWalletName)];
};

const isValidCurrency = ({ field }) => {
  const isValid = field.value === 'ada';
  return [isValid, intl.formatMessage(messages.invalidCurrency)];
};

const fields = {
  walletName: {
    value: '',
    validate: [isValidWalletName]
  },
  currency: {
    value: 'ada',
    validate: [isValidCurrency]
  },
};

const options = {
  validateOnChange: false
};

class WalletCreateForm extends MobxReactForm {

  onSuccess(form) {
    state.walletsStore.createPersonalWallet(form.values());
  }

  onError(form) {
    // get all form errors
    console.log('All form errors', form.errors(), this); // eslint-disable-line
  }
}

export default new WalletCreateForm({ options, fields });
