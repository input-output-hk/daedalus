import path from 'path';
import fs from 'fs';
import log from 'electron-log';
import { launcherConfig } from './launcherConfig';
import { runtimeFolderPath } from '../config';

const isProd = process.env.NODE_ENV === 'production';
const caDevelopmentPath = process.env.CARDANO_TLS_PATH || '';

if (!isProd && !caDevelopmentPath) {
  throw new Error('Environment variable missing: CARDANO_TLS_PATH');
}

/**
 * Here we are reading the TLS certificate from the file system
 * and make it available to render processes via a global variable
 * so that it can be used in HTTP and Websocket connections.
 */
export const setupTls = () => {
  const tlsBasePath = launcherConfig.tlsPath || path.join(runtimeFolderPath, 'tls');
  const caProductionPath = path.join(tlsBasePath, 'client', 'ca.crt');
  const pathToCertificate = isProd ? caProductionPath : path.join(caDevelopmentPath, 'ca.crt');

  try {
    log.info('Using certificates from: ' + pathToCertificate);
    Object.assign(global, {
      ca: fs.readFileSync(pathToCertificate),
    });
  } catch (error) {
    log.error(`Error while loading ca.crt: ${error}`);
  }
};
