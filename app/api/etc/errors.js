import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';

const messages = defineMessages({
  noKeyForGivenAddress: {
    id: 'api.errors.etc.NoKeyForGivenAddress',
    defaultMessage: '!!!No key found for given address.',
    description: 'Error message that is displayed when no key was found for a given address.'
  },
  accountLockedOrUnknown: {
    id: 'api.errors.etc.AccountLockedOrUnknown',
    defaultMessage: '!!!Account is locked or unknown',
    description: 'Error message that is displayed when account is locked or unknown.'
  },
});

export class NoKeyForGivenAddress extends LocalizableError {
  constructor() {
    super({
      id: messages.noKeyForGivenAddress.id,
      defaultMessage: messages.noKeyForGivenAddress.defaultMessage,
    });
  }
}

export class AccountLockedOrUnknown extends LocalizableError {
  constructor() {
    super({
      id: messages.accountLockedOrUnknown.id,
      defaultMessage: messages.accountLockedOrUnknown.defaultMessage,
    });
  }
}
