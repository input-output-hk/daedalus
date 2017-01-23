import { defineMessages } from 'react-intl';

export default defineMessages({
  fieldIsRequired: {
    id: 'global.errors.fieldIsRequired',
    defaultMessage: '!!!This field is required.',
    description: 'Error message when required fields are left empty.'
  },
  invalidWalletName: {
    id: 'global.errors.invalidWalletName',
    defaultMessage: '!!!The wallet name must have at least 3 letters.',
    description: 'Error message shown when invalid wallet name was entered in create wallet dialog.'
  },
});
