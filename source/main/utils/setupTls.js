import path from 'path';
import fs from 'fs';
import log from 'electron-log';
import { launcherConfig } from './launcherConfig';
import { runtimeFolderPath } from '../config';

const isProd = process.env.NODE_ENV === 'production';
const caDevelopmentPath = process.env.CARDANO_TLS_PATH || '';

if (!isProd && !(caDevelopmentPath || launcherConfig.tlsPath)) {
  throw new Error('Environment variable missing: CARDANO_TLS_PATH');
}

/**
 * Here we are reading the TLS certificate from the file system
 * and make it available to render processes via a global variable
 * so that it can be used in HTTP and Websocket connections.
 */
export const setupTls = () => {
  const tlsBasePath = launcherConfig.tlsPath || path.join(runtimeFolderPath, 'tls');
  const tlsFolder = isProd ? path.join(tlsBasePath, 'client') : caDevelopmentPath;
  const pathToCa = path.join(tlsFolder, 'ca.crt');
  const pathToClientKey = path.join(tlsFolder, 'client.key');
  const pathToClientCert = path.join(tlsFolder, 'client.pem');

  try {
    log.info('Using certificates from: ' + tlsFolder);
    Object.assign(global, {
      ca: fs.readFileSync(pathToCa),
      clientKey: fs.readFileSync(pathToClientKey),
      clientCert: fs.readFileSync(pathToClientCert),
    });
  } catch (error) {
    log.error(`Error while loading tls files: ${error}`);
  }
};
