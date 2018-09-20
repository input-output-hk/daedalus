// @flow
import path from 'path';
import fs from 'fs';
import log from 'electron-log';
import { runtimeFolderPath } from '../config';
import type { TlsConfig } from '../../common/types/cardanoNode.types';

const isProd = process.env.NODE_ENV === 'production';
const caDevelopmentPath = process.env.CARDANO_TLS_PATH || '';

if (!isProd && !caDevelopmentPath) {
  throw new Error('Environment variable missing: CARDANO_TLS_PATH');
}

/**
 * Here we are reading the TLS certificate from the file system and returns them
 */
export const loadTlsConfig = (tlsPath?: ?string): TlsConfig => {
  const tlsBasePath = tlsPath || path.join(runtimeFolderPath, 'tls');
  const tlsFolder = isProd ? path.join(tlsBasePath, 'client') : caDevelopmentPath;
  const pathToCa = path.join(tlsFolder, 'ca.crt');
  const pathToClientKey = path.join(tlsFolder, 'client.key');
  const pathToClientCert = path.join(tlsFolder, 'client.pem');

  log.info('Loading tls certificates from: ' + tlsFolder);
  return {
    ca: fs.readFileSync(pathToCa),
    cert: fs.readFileSync(pathToClientCert),
    key: fs.readFileSync(pathToClientKey),
    port: 8090,
  };
};
