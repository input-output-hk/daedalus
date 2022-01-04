import ExtendableError from 'es6-error';

export class TlsCertificateNotValidError extends ExtendableError {
  static API_ERROR = 'CERT_NOT_YET_VALID';
}
