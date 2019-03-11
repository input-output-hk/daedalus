// @flow
import { defineMessages } from 'react-intl';
import LocalizableError from '../../i18n/LocalizableError';

const messages = defineMessages({
  tlsCertificateNotValidError: {
    id: 'api.nodes.errors.TlsCertificateNotValidError',
    defaultMessage: '!!!TLS certificate is not valid',
    description: 'Error message warning that the TLS cert is not valid'
  },
});

export class TlsCertificateNotValidError extends LocalizableError {
  static API_ERROR = 'CERT_NOT_YET_VALID';
  constructor() {
    super({
      id: messages.tlsCertificateNotValidError.id,
      defaultMessage: messages.tlsCertificateNotValidError.defaultMessage,
    });
  }
}
