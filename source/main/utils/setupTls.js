import path from 'path';
import fs from 'fs';
import Log from 'electron-log';

const isProd = process.env.NODE_ENV === 'production';

/**
 * Here we are reading the TLS certificate from the file system
 * and make it available to render processes via a global variable
 * so that it can be used in HTTP and Websocket connections.
 */
export const setupTls = () => {
  const caProductionPath = path.join(process.cwd(), 'tls', 'ca', 'ca.crt');
  const pathToCertificate = isProd ? caProductionPath : path.join(process.cwd(), 'tls', 'ca.crt');

  try {
    Log.info('Using certificates from: ' + pathToCertificate);
    Object.assign(global, {
      ca: fs.readFileSync(pathToCertificate),
    });
  } catch (error) {
    Log.error(`Error while loading ca.crt: ${error}`);
  }
};
