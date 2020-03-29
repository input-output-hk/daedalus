import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';

const messages = defineMessages({
  notAllowedToSendMoneyToSameAddressError: {
    id: 'api.errors.NotAllowedToSendMoneyToSameAddressError',
    defaultMessage:
      "!!!It's not allowed to send money to the same address you are sending from. Make sure you have enough addresses with money in this account or send to a different address.",
    description:
      '"It\'s not allowed to send money to the same address you are sending from." error message.',
  },
  notAllowedToSendMoneyToRedeemAddressError: {
    id: 'api.errors.NotAllowedToSendMoneyToRedeemAddressError',
    defaultMessage:
      '!!!It is not allowed to send money to ada redemption address.',
    description:
      '"It is not allowed to send money to ada redemption address." error message.',
  },
  notEnoughMoneyToSendError: {
    id: 'api.errors.NotEnoughMoneyToSendError',
    defaultMessage: '!!!Not enough money to make this transaction.',
    description: '"Not enough money to make this transaction." error message.',
  },
  allFundsAlreadyAtReceiverAddressError: {
    id: 'api.errors.AllFundsAlreadyAtReceiverAddressError',
    defaultMessage:
      '!!!All your funds are already at the address you are trying send money to.',
    description:
      '"All your funds are already at the address you are trying send money to." error message.',
  },
  notEnoughFundsForTransactionFeesError: {
    id: 'api.errors.NotEnoughFundsForTransactionFeesError',
    defaultMessage: '!!!Not enough ada for fees. Try sending a smaller amount.',
    description:
      '"Not enough ada for fees. Try sending a smaller amount." error message',
  },
  notEnoughFundsForTransactionError: {
    id: 'api.errors.NotEnoughFundsForTransactionError',
    defaultMessage: '!!!Not enough ada . Try sending a smaller amount.',
    description:
      '"Not enough ada . Try sending a smaller amount." error message',
  },
  canNotCalculateTransactionFeesError: {
    id: 'api.errors.CanNotCalculateTransactionFeesError',
    defaultMessage:
      '!!!Cannot calculate fees while there are pending transactions.',
    description:
      '"Cannot calculate fees while there are pending transactions." error message',
  },
  tooBigTransactionError: {
    id: 'api.errors.TooBigTransactionError',
    defaultMessage: '!!!Transaction too big due to too many inputs.',
    description: '"Transaction too big due to too many inputs." error message.',
  },
  tooBigTransactionErrorLinkLabel: {
    id: 'api.errors.TooBigTransactionErrorLinkLabel',
    defaultMessage: '!!!Learn more.',
    description:
      '"Transaction too big due to too many inputs." error link label.',
  },
  tooBigTransactionErrorLinkURL: {
    id: 'api.errors.TooBigTransactionErrorLinkURL',
    defaultMessage:
      '!!!https://iohk.zendesk.com/hc/en-us/articles/360017733353',
    description:
      '"Transaction too big due to too many inputs." error link URL.',
  },
  invalidAddressError: {
    id: 'api.errors.invalidAddress',
    defaultMessage: '!!!Please enter a valid address.',
    description: 'Error message shown when invalid address was entered.',
  },
});

export class NotAllowedToSendMoneyToSameAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.notAllowedToSendMoneyToSameAddressError.id,
      defaultMessage:
        messages.notAllowedToSendMoneyToSameAddressError.defaultMessage,
    });
  }
}

export class NotAllowedToSendMoneyToRedeemAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.notAllowedToSendMoneyToRedeemAddressError.id,
      defaultMessage:
        messages.notAllowedToSendMoneyToRedeemAddressError.defaultMessage,
    });
  }
}

export class NotEnoughMoneyToSendError extends LocalizableError {
  constructor() {
    super({
      id: messages.notEnoughMoneyToSendError.id,
      defaultMessage: messages.notEnoughMoneyToSendError.defaultMessage,
    });
  }
}

export class AllFundsAlreadyAtReceiverAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.allFundsAlreadyAtReceiverAddressError.id,
      defaultMessage:
        messages.allFundsAlreadyAtReceiverAddressError.defaultMessage,
    });
  }
}

export class NotEnoughFundsForTransactionFeesError extends LocalizableError {
  constructor() {
    super({
      id: messages.notEnoughFundsForTransactionFeesError.id,
      defaultMessage:
        messages.notEnoughFundsForTransactionFeesError.defaultMessage,
    });
  }
}

export class NotEnoughFundsForTransactionError extends LocalizableError {
  constructor() {
    super({
      id: messages.notEnoughFundsForTransactionError.id,
      defaultMessage: messages.notEnoughFundsForTransactionError.defaultMessage,
    });
  }
}

export class CanNotCalculateTransactionFeesError extends LocalizableError {
  constructor() {
    super({
      id: messages.canNotCalculateTransactionFeesError.id,
      defaultMessage:
        messages.canNotCalculateTransactionFeesError.defaultMessage,
    });
  }
}

export class TooBigTransactionError extends LocalizableError {
  constructor() {
    super({
      id: messages.tooBigTransactionError.id,
      defaultMessage: messages.tooBigTransactionError.defaultMessage,
      values: {
        linkLabel: messages.tooBigTransactionErrorLinkLabel,
        linkURL: messages.tooBigTransactionErrorLinkURL,
      },
    });
  }
}

export class InvalidAddressError extends LocalizableError {
  constructor() {
    super({
      id: messages.invalidAddressError.id,
      defaultMessage: messages.invalidAddressError.defaultMessage,
    });
  }
}
