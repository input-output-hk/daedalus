import path from 'path';
import fs from 'fs';
import Log from 'electron-log';

/**
 * Here we are reading the TLS certificate from the file system
 * and make it available to render processes via a global variable
 * so that it can be used in HTTP and Websocket connections.
 */
export const setupTls = () => {
  const pathToCertificate = path.join(process.cwd(), 'tls', 'ca', 'ca.crt');

  try {
    Log.info('Using certificates from: ' + pathToCertificate);
    Object.assign(global, {
      ca: fs.readFileSync(pathToCertificate),
    });
  } catch (error) {
    Log.error(`Error while loading ca.crt: ${error}`);
  }
};
