import path from 'path';
import ClientApi from 'daedalus-client-api';

export default function getTlsConfig() {
  return ClientApi.tlsInit(path.join(__dirname, '../../tls/ca.crt'));
}
